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
-- Evaluation

type Env = [(String, Value)]

newtype Evaluation r = Evaluation {
  runEval :: [Fun] -> Env -> (r, Env)
}

data Value = ValueInt Int | ValueDouble Double

instance (Monad m) => Evaluation m where
  return :: a -> Evaluation a
  return r = Evaluation $ \_ -> \e -> (r, e)

  bind :: Evaluation a -> (a -> Evaluation b) -> Evaluation b
  bind e f = Evaluation $ \fs -> \env ->
    runEval (f a) fs newenv where
      (a, newenv) = runEval e fs env

run :: [Fun] -> Int

run funs = let 
  main_funcs = filter (\f. name f == "main") funs in
  if len main_funcs == 1 then
    evalf . getStmts . car main_funcs
  else
    error "There are 0 or more than one main functions."

evalf :: [Stmt] -> Evaluation Value
evalf (Return expr) :: _ = evale expr

lookup :: String -> Env -> Value
lookup x [] = error "No variable found"
lookup x (y,v):xs = if x == y then v else lookup x xs

update :: String -> Value -> Env -> Env
update x v [] = []
update x v (y,w):xs = if x == y 
  then (y,v):xs
  else update x w xs

v2d :: Value -> Double
v2d (ValueInt i) = fromIntegral i
v2d (ValueDouble d) = d

evale :: Expr -> Evaluation Value
evale (Var v) = Evaluation $ (\_ -> \e -> (lookup v e, e))

evale (LitI i) = return . ValueInt i
evale (LitF d) = return . ValueDouble d
evale (Add e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  case v1, v2 of
    (ValueInt i1, ValueInt i2) -> ValueInt (i1 + i2)
    _, _ => ValueDouble (v2d v1 + v2d v2)

evale (Sub e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  case v1, v2 of
    (ValueInt i1, ValueInt i2) -> ValueInt (i1 - i2)
    _, _ => ValueDouble (v2d v1 - v2d v2)

evale (Mul e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  case v1, v2 of
    (ValueInt i1, ValueInt i2) -> ValueInt (i1 * i2)
    _, _ => ValueDouble (v2d v1 * v2d v2)

evale (Div e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  case v1, v2 of
    (ValueInt i1, ValueInt i2) ->
      if i2 == 0
        then error "Zero division!"
        else ValueInt (i1 / i2)
    _, _ => ValueDouble (v2d v1 / v2d v2)

inc :: Value -> Value
inc (ValueInt i) = ValueInt (i + 1)
inc _ = error "you cannot increase noninteger."

evale (Inc e) = do
  x <- evale_lval e
  v <- evale x
  Evaluation $ \_ -> \env -> (v, update (name x) (inc v) env)

evale (Assgn l r) = do
  x <- evale_lval l
  v <- evale r
  Evaluation $ \_ -> \env -> (v, update (name x) v env)

evale (Lt e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  let b = case v1, v2 of
    (ValueInt i1, ValueInt i2) -> i1 < i2
    _, _ -> v2d v1 < v2d v2 in
  if b then ValueInt 1 else ValueInt 0

evale (Gt e1 e2) = do
  v1 <- evale e1
  v2 <- evale e2
  let b = case v1, v2 of
    (ValueInt i1, ValueInt i2) -> i1 > i2
    _, _ -> v2d v1 > v2d v2 in
  if b then ValueInt 1 else ValueInt 0

buildEnv :: [(Type, String)] -> [Value] -> Env
buildEnv a b = do
  (t, s) <- a
  v <- b
  return (s, v)

call :: [Stmt] -> Env -> Evaluation Value
call body callenv = Evaluation $ \fs -> \env -> 
  runEval e fs (callenv:env) where
    e = evalf body

evale (Call fname args) = do 
  vargs <- forM args evale
  Evaluation $ \fs -> \env ->
    let filtered = filter (\f -> (name f) == fname) fs in
    if (len filtered) != 1
      then error "No such function"
      else
        let f = car filtered in
        let callenv = buildEnv (arguments f) vargs in
        let e = evalf body in
          runEval e fs (callenv:env)

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