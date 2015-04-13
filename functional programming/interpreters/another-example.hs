import Data.Char
import Data.String

class Parse a where
  parse :: Parser a
  fromString :: String -> a
  fromString cs =
    case parse cs of
            Just (s, []) -> s
            Just (s, cs) -> error ("garbage '"++cs++"'")
            Nothing -> error "Nothing"
  toString :: a -> String

data Expr =
  Num Int | Var String | Add (Expr, Expr) |
  Sub (Expr,Expr) | Mul (Expr, Expr) | Div (Expr, Expr)

data Statement = Assignment String Expr

type Parser a = String -> Maybe (a, String)

--a set of simple expression parsing functions
var :: Parser Expr
var = word >-> Var

num :: Parser Expr
num = number >-> Num

factor :: Parser Expr
factor = num ! var ! lit '(' -# var #- lit ')' ! err "illegal factor"

term :: Parser Expr
term = factor #> term'

expr = term #> expr'

mulOp :: Parser ((Expr, Expr) -> Expr)
mulOp = lit '*' >-> (\_ -> Mul)
      ! lit '/' >-> (\_ -> Div)
addOp = lit '+' >-> (\_ -> Add)
      ! lit '-' >-> (\_ -> Sub)

term' :: Expr -> Parser Expr
term' e =
   mulOp # factor >-> bldOp e #> term' !
   return' e

expr' :: Expr -> Parser Expr
expr' e =
  addOp # term >-> bldOp e #> expr' !
  return' e

bldOp :: Expr -> ((Expr, Expr) -> Expr, Expr) -> Expr
bldOp e (oper, e') = oper (e,e')

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

--Various operators that make our lives easier
--passes the result and remainder of first parser to the second
infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> k) cs =
  case m cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'

--Basically an 'or' operator
infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs =
  case m cs of
    Nothing -> n cs
    mcs     -> mcs

--the >-> operator takes a transformation and returns a new parser
infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs =
  case m cs of
    Just(a,cs') -> Just(k a, cs')
    Nothing -> Nothing

--Take the first or second of a pair
infixr 4 -#
(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = ((m # n) >-> snd) cs

infixl 5 #-
(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = ((m # n) >-> fst) cs

--the # operator applies two parsers in sequence
infix 6 #
(#) :: Parser a -> Parser b -> Parser (a,b)
(m # n) cs =
  case m cs of
    Nothing -> Nothing
    Just(p, cs') ->
      case n cs' of
        Nothing -> Nothing
        Just(q,cs'') -> Just ((p,q),cs'')

--Checks a boolean against a parser result.
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
  case m cs of
    Nothing -> Nothing
    Just(a,cs) -> if p a then Just (a,cs) else Nothing

--Tiny baby parsers that we'll use to build up more complex ones slowly:
becomes :: Parser String
becomes cs = (twochars ? (==":=")) cs

char :: Parser Char
char (c:cs) = Just (c,cs)
char []     = Nothing

digit :: Parser Char
digit = (char ? isDigit)

letter :: Parser Char
letter = (char ? isAlpha)

space :: Parser Char
space = (char ? isSpace)

fail' :: Parser a
fail' cs = Nothing

return' :: a -> Parser a
return' a cs = Just (a, cs)

lit :: Char -> Parser Char
lit c = char ? (==c)

alphanum :: Parser Char
alphanum c = (digit ! letter) c

twochars :: Parser String
twochars = ((char # char) >-> (\x -> (fst x):(snd x):[]))

digitVal :: Parser Int
digitVal = digit >-> digitToInt

upperVal :: Parser Char
upperVal = char >-> toUpper

sndchar :: Parser Char
sndchar = twochars >-> last

iterate' :: Parser a -> Int -> Parser [a]
iterate' m 0 = return' []
iterate' m i = m # iterate' m (i-1) >-> cons

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return' []

letters :: Parser String
letters = letter # iter letter >-> cons

token :: Parser a -> Parser a
token m = m #- iter space

word :: Parser String
word = token letters

accept :: String -> Parser String
accept w = token (iterate' char (length w) ? (==w))

double :: Parser Char
double = char #> lit

bldNumber :: Int -> Int -> Int
bldNumber n d = 10*n + d

number' :: Int -> Parser Int
number' n =
  digitVal >-> bldNumber n #> number' ! return' n

number :: Parser Int
number = token (digitVal #> number')
