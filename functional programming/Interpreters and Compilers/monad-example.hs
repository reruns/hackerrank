--Same as example.hs, but uses Monads
import Data.List
import Data.Function
import Control.Monad

--This stuff is identical
type Var = String

infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Exp
  = C Int
  | V Var
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Exp :/: Exp

infix 1 :=

data Stmt
  = Var := Exp
  | While Exp Stmt
  | Seq [Stmt]

type Prog = Stmt
type Val = Int
type Store = [(Var, Val)]

--contruct a composite monad for dealing with the store and handling errors
newtype Interp a = Interp {runInterp :: Store -> Either String (a, Store) }
instance Monad Interp where
  return x = Interp $ \r -> Right (x,r)
  i >>= k  = Interp $ \r -> case runInterp i r of
                Left msg      -> Left msg
                Right (x,r')  -> runInterp (k x) r'
  fail msg = Interp $ \_ -> Left msg

--Reading and writing to the store
rd :: Var -> Interp Val
rd x = Interp $ \r -> case lookup x r of
         Nothing -> Left ("unbound variable `" ++ x ++ "'")
         Just v  -> Right (v, r)

wr :: Var -> Val -> Interp ()
wr x v = Interp $ \r -> Right ((), (x, v) : r)

--expression evaluator
eval :: Exp -> Interp Val
eval (C n)        = do return n
eval (V x)        = do rd x
eval (e1 :+: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 + v2)
eval (e1 :-: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 - v2)
eval (e1 :*: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       return (v1 * v2)
eval (e1 :/: e2)  = do v1 <- eval e1
                       v2 <- eval e2
                       if v2 == 0
                        then fail "division by zero"
                        else return (v1 `div` v2)

--statements do not result in values but are executed only
--for the effects on the store
exec :: Stmt -> Interp ()
exec (x := e)       = do v <- eval e
                         wr x v
exec (While e s)    = do v <- eval e
                         when (v /= 0) (exec (Seq [s, While e s]))
exec (Seq [])       = do return ()
exec (Seq (s : ss)) = do exec s
                         exec (Seq ss)

--run performs a monadic computation
run :: Prog -> Store -> Either String Store
run p r = case runInterp (exec p) r of
            Left msg      -> Left msg
            Right (_,r')  -> Right (nubBy ((==) `on` fst) r')

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
