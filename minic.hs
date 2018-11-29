-- mini-C compiler

module MyCompiler where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Language
import Text.Parsec.Token
import Text.Parsec.Expr
import Control.Applicative hiding ((<|>), many)

--------------------------------------------------------------------------------
-- Lexer

lang = emptyDef {identStart=letter <|> char '_',
                 identLetter=alphaNum <|> char '_',
                 reservedNames=["int", "float", "for", "if", "void", "return"],
                 reservedOpNames=["++","*","/","+","-","<",">","=",";"]}

lexer = makeTokenParser lang

paren = parens lexer
brace = braces lexer
bracket = brackets lexer
number = natural lexer
ident = identifier lexer
literal = naturalOrFloat lexer
keyword = reserved lexer
op = reservedOp lexer

--------------------------------------------------------------------------------
-- Parser

getPos = (sourceLine . statePos) <$> getParserState
--------------------------------------------------------------------------------
-- Expressions

data Expr = Var String | LitI Integer | LitF Double | Add Expr Expr
          | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Inc Expr
          | Deref Expr | Index Expr Expr | Assgn Expr Expr
          | Lt Expr Expr | Gt Expr Expr | Call String [Expr] deriving (Show)

p_expr = (flip buildExpressionParser) p_term [
  [Postfix (op "++" >> return Inc)],
  [Prefix (op "*" >> return Deref)],
  {- very clever idea from
    https://stackoverflow.com/questions/17287578/how-to-implement-subscript-operators-in-parsec
    see comments of OP's question -}
  [Postfix $ (flip Index) <$> (bracket p_expr)],
  [
    Infix (op "*" >> return Mul) AssocLeft,
    Infix (op "/" >> return Div) AssocLeft
  ],
  [
    Infix (op "+" >> return Add) AssocLeft,
    Infix (op "-" >> return Sub) AssocLeft
  ],
  [
    Infix (op "<" >> return Lt) AssocLeft,
    Infix (op ">" >> return Gt) AssocLeft
  ],
  [Infix (op "=" >> return Assgn) AssocRight]]

p_literal = do
  lit <- literal
  case lit of
    Left i -> return $ LitI i
    Right d -> return $ LitF d

p_call = Call <$> ident <*> paren (p_expr `sepBy` op ",")

p_term = p_literal <|> paren (p_expr) <|> try (p_call) <|> Var <$> ident

--------------------------------------------------------------------------------
-- Statements

data Stmt = For {
  line :: Int,
  init :: Maybe Expr,
  flag :: Maybe Expr,
  rept :: Maybe Expr,
  body :: [Stmt]

} | If {
  line :: Int,
  cond :: Expr,
  body :: [Stmt]
} | VarDecl {
  line :: Int,
  t_smpl :: Tsmpl,
  names :: [DecoratedVarName]
} | Return {
  line :: Int,
  expr :: Expr
} | Expr {
  line :: Int,
  expr :: Expr
} | Mpty deriving (Show)

data DecoratedVarName = Smpl String | Pointer String
                      | Array String Integer deriving (Show)

data Tsmpl = Tint | Tfloat deriving (Show)
data Type = Tsmpl Tsmpl | Tarray Tsmpl Integer | Tpointer Tsmpl deriving (Show)
                      
(<&>) a b = (,) <$> a <*> b

p_for_conds = optionMaybe p_expr <&> (op ";" *> optionMaybe p_expr) <&>
                  (op ";" *> optionMaybe p_expr)

p_for = do
  keyword "for"
  line <- getPos
  ((init, cond), rept) <- paren (p_for_conds)
  body <- p_stmts <|> (:[]) <$> p_stmt
  return $ For line init cond rept body

p_if = keyword "if" *> 
       (If <$> getPos <*> paren (p_expr) <*> (p_stmts <|> (:[]) <$> p_stmt))

p_deco_name = op "*" *> (Pointer <$> ident) 
            <|> try (Array <$> ident <*> bracket(number))
            <|> Smpl <$> ident

p_type_smpl = keyword "int" *> return Tint <|> keyword "float" *> return Tfloat
p_var_decl = VarDecl <$> getPos <*> p_type_smpl <*> (p_deco_name `sepBy` op ",")
            <* op ";"

p_return = keyword "return" *> (Return <$> getPos <*> p_expr <* op ";")
p_empty_stmt = do op ";" ; return Mpty

p_stmt = p_for <|> p_if <|> p_var_decl 
      <|> p_return <|> p_empty_stmt <|> Expr <$> getPos <*> p_expr <* op ";"
p_stmts = brace (many p_stmt)

--------------------------------------------------------------------------------
-- Functions

data Fun = Fun {
  name :: String,
  arguments :: [(Type, String)],
  ret_type :: Type,
  fun_body :: [Stmt]
} deriving (Show)

p_func_ret_t = try (Tpointer <$> p_type_smpl <* op "*") <|> Tsmpl <$> p_type_smpl
p_func_arg = (,) <$> p_func_ret_t <*> ident
          <|> do
            t <- p_type_smpl
            name <- ident
            len <- bracket (number)
            return (Tarray t len, name)

p_func_args = paren (keyword "void" *> (return []) <|> p_func_arg `sepBy` op ",")

p_func = do
  ret_t <- p_func_ret_t
  name <- ident
  args <- p_func_args
  body <- p_stmts
  return $ Fun name args ret_t body

p_program = spaces *> many p_func

parseProgram = parse p_program ""
--------------------------------------------------------------------------------
ex_program = "int avg(int count, int *value) {\n\
\  int i, total;;\n\
\  for (i = 1; i < count; i++) {\n\
\    total = toal + value[i];\n\
\  }\n\
\\n\
\  return (total / count);\n\
\}\n\
\\n\
\int main(void) {\n\
\  int studentNumber, count, i, sum;\n\
\  int mark[4];\n\
\  float average;\n\
\\n\
\  count = 4;\n\
\  sum = 0;\n\
\\n\
\  for (i = 0; i < count; i++) {\n\
\    mark[i] = i * 30;\n\
\    sum = sum + mark[i];\n\
\    average = avg(i + 1, mark);\n\
\    if (average > 40) {\n\
\      printf(f, average);\n\
\    }\n\
\  }\n\
\}"

ex_p2 = " int avg ( int count , int * value ) {\n\
\  int i , total ; ; \n\
\  for ( i = 1 ; i < count ; i ++ ) {\n\
\    total = toal + value [ i ] ;\n\
\  }\n\
\\n\
\  return ( total / count ) ;\n\
\}\n\
\\n\
\int main ( void ) { \n\
\  int studentNumber , count , i , sum ;\n\
\  int mark [ 4 ] ;\n\
\  float average ;\n\
\\n\
\  count = 4;\n\
\  sum = 0;\n\
\\n\
\  for ( i = 0 ; i < count ; i ++ ) {\n\
\    mark [ i ] = i * 30 ;\n\
\    sum = sum + mark [ i ] ;\n\
\    average = avg ( i + 1 , mark ) ;\n\
\    if ( average > 40 ) {\n\
\      printf( f , average ) ;\n\
\    }\n\
\  }\n\
\}"
ex3 = " int avg ( int count , int * value ) {\n\
\  int i , total ; ; \n\
\  for ( i = 1 ; i < count ; i ++ ) {\n\
\    total = toal + value [ i ] ;\n\
\  }\n\
\\n\
\  return ( total / count ) ;\n\
\}"