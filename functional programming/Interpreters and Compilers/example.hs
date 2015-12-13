--an interpreter for a very simple interpreted language
import Data.List
import Data.Function

--mutable variables can just be represented by a string
type Var = String

--These are our expressions
infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = C Int
  | V Var
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Exp :/: Exp


--our various kinds of statements
infix 1 :=

data Stmt
  = Var := Exp
  | While Exp Stmt
  | Seq [Stmt]

--A program is just a statement
type Prog = Stmt

--The interpreter needs to keep track of the assigned values
--To emulate 'memory', we'll just use a list of tuples
type Val = Int
type Store = [(Var, Val)]

--This is the expression evaluator
--Map constants to their value
--Lookup variables in the store
--Map arithmetic operations to their Haskell counterparts
eval :: Exp -> Store -> Val
eval (C n) r        = n
eval (V x) r        = case lookup x r of
                        Nothing -> error ("unbound variable '" ++ x ++ "'")
                        Just v  -> v
eval (e1 :+: e2) r  = eval e1 r + eval e2 r
eval (e1 :-: e2) r  = eval e1 r - eval e2 r
eval (e1 :*: e2) r  = eval e1 r * eval e2 r
eval (e1 :/: e2) r  = eval e1 r `div` eval e2 r

--Executing a statement takes a store as an argument and yields the updated store
exec :: Stmt -> Store -> Store
exec (x := e) r                    = (x, eval e r) : r
exec (While e s) r | eval e r /= 0 = exec (Seq [s, While e s]) r
                   | otherwise     = r
exec (Seq []) r                    = r
exec (Seq (s : ss)) r              = exec (Seq ss) (exec s r)

--Running a program is just executing its top level statement
--with an initialized store
run :: Prog -> Store -> Store
run p r = nubBy ((==) `on` fst) (exec p r)

--an example program to compute the nth Fibonacci number
fib :: Prog
fib = Seq
  [ "x" := C 0
    ,"y" := C 1
    , While (V "n") $ Seq
      [ "z" := V "x" :+: V "y"
      , "x" := V "y"
      , "y" := V "z"
      , "n" := V "n" :-: C 1
      ]
  ]
