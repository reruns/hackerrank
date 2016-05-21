import Data.Char
import Control.Monad hiding (return)
import Prelude hiding(return, fail)

data Expr = Num Int | Var Int Char Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

type Parser a = String -> Maybe (a, String)

instance Show Expr where
    show = showExpr
    
    
--and then the actual stuff that runs in HackerRank
solve :: String -> String
solve x = case expr (process x) of
            Nothing -> ""
            Just (a,b) -> showPoly $ eval a
            
main = do
    t <- readLn
    replicateM_ t $ do
        s <- getLine
        putStrLn $ solve s
    
--evaluation
eval :: Expr -> [(Int, Char, Int)]
eval (Num a) = [(a, 'x', 0)]
eval (Var a c b) = [(a,c,b)]
eval (Add a b) = addP (eval a) (eval b)
eval (Sub a b) = subP (eval a) (eval b)
eval (Mul a b) = mulP (eval a) (eval b)
eval (Div a b) = divP (eval a) (eval b)

--add
addP :: [(Int, Char, Int)] -> [(Int, Char, Int)] -> [(Int, Char, Int)]
addP [] [] = []
addP xs [] = xs
addP [] ys = ys
addP (x@(a,b,c):xs) (y@(d,e,f):ys) | c > f = x : (addP xs (y:ys))
                                   | c < f = y : (addP (x:xs) ys)
                                   | otherwise = (a+d,b,c) : (addP xs ys)
                                   
--subtract
subP :: [(Int, Char, Int)] -> [(Int, Char, Int)] -> [(Int, Char, Int)]
subP [] [] = []
subP xs [] = xs
subP [] ys = map (\(a,b,c) -> (-a,b,c)) ys
subP (x@(a,b,c):xs) (y@(d,e,f):ys) | c > f = x : (subP xs (y:ys))
                                   | c < f = (-d,e,f) : (subP (x:xs) ys)
                                   | otherwise = (a-d,b,c):(subP xs ys)
          
--multiply
mulP :: [(Int, Char, Int)] -> [(Int, Char, Int)] -> [(Int, Char, Int)]
mulP x y = foldr addP [] [[(a*d,b,c+f)] | (a,b,c) <- x, (d,e,f) <- y]
   
--divide
--We're cheating a bit here, since denominators always evaluate to integers
divP :: [(Int, Char, Int)] -> [(Int, Char, Int)] -> [(Int, Char, Int)]
divP a [(b,_,_)] = map (\(x,y,z) -> (x `div` b,y,z)) a



--The combined parser functions
mulOp, addOp :: Parser (Expr -> Expr -> Expr)
mulOp = lit '*' >-> (\_ -> Mul) ! lit '/' >-> (\_ -> Div)
addOp = lit '+' >-> (\_ -> Add) ! lit '-' >-> (\_ -> Sub)

--Maybe a number, a character, maybe ^Number
var = bth ! noxp ! jxp ! nthr
noxp = number # letter >-> (\(a,b) -> Var a b 1)
bth = number # letter # lit '^' # number >-> (\x -> Var (fst$fst$fst x) (snd$fst$fst x) (snd x))
jxp = letter # lit '^' # number >-> (\x -> Var 1 (fst$fst x) (snd x))
nthr = letter >-> (\x -> Var 1 x 1)

num :: Parser Expr
num = (ldmin ! number) >-> Num

factor :: Parser Expr
factor = var ! num ! lit '(' -# expr #- lit ')' ! err "illegal factor"

term' :: Expr -> Parser Expr
term' e = mulOp # factor >-> bldOp e #> term' ! return e

term :: Parser Expr
term = factor #> term'

expr' :: Expr -> Parser Expr
expr' e = addOp # term >-> bldOp e #> expr' ! return e

expr :: Parser Expr
expr = term #> expr'

--simple parser components
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

return :: a -> Parser a
return a cs = Just (a, cs)

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
iterate' m 0 = return []
iterate' m i = m # iterate' m (i-1) >-> cons

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd:tl

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

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

number' :: Int -> Parser Int
number' n =
  digitVal >-> bldNumber n #> number' ! return n

number :: Parser Int
number = token (digitVal #> number')

ldmin :: Parser Int
ldmin = lit '-' # number >-> (\x -> -(snd x))

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")

--Parser Operators
-- ? applies parser m iff p a is true
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = case m cs of
                Nothing -> Nothing
                Just (a,cs) -> if p a then Just(a,cs) else Nothing
                
-- ! applies parser m. If m fails, it applies parser n.
infixl 3 !
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
                Nothing -> n cs
                mcs -> mcs

--combines two parsers in sequence where the remainder of one is fed into the other        
infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a,b)
(m # n) cs = case m cs of
                Nothing -> Nothing
                Just (p, cs') -> case n cs' of
                                    Nothing -> Nothing
                                    Just(q, cs'') -> Just((p,q), cs'')

--Transform the result of a parser with k                                    
infixl 5 >->
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> k) cs = case m cs of
                   Just(a, cs') -> Just(k a, cs')
                   Nothing -> Nothing

--Make the result of one parser available to another
infix 4 #>
(#>) :: Parser a -> (a -> Parser b) -> Parser b
(m #> k) cs = case m cs of
                Nothing -> Nothing
                Just (a, cs') -> k a cs'
            
infixl 7 -#, #-            
(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = ((m # n) >-> snd) cs

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = ((m # n) >-> fst) cs

--other helpers
bldNumber :: Int -> Int -> Int
bldNumber n d = 10*n+d

bldOp :: Expr -> (Expr -> Expr -> Expr, Expr) -> Expr
bldOp e (oper, e') = oper e e'

process = fix.clean.trim

fix :: String -> String
fix ('x':'(':xs) = "x*(" ++ fix xs
fix (x:xs) = x : fix xs
fix [] = []

clean :: String -> String
clean ('-':xs) = "-1*"++xs
clean xs = xs

trim :: String -> String
trim [] = []
trim (' ':xs) = trim xs
trim (x:xs) = x : trim xs

showExpr (Num a) = show a
showExpr (Add a b) = (showExpr a) ++ "+" ++ (showExpr b)
showExpr (Sub a b) = (showExpr a) ++ "-" ++ (showExpr b)
showExpr (Mul a b) = (showExpr a) ++ "*" ++ (showExpr b)
showExpr (Div a b) = (showExpr a) ++ "/" ++ (showExpr b)
showExpr (Var 0 _ _) = []
showExpr (Var a _ 0) = (if (a < 0) then (show (-a)) else show a) 
showExpr (Var 1 c 1) = [c]
showExpr (Var (-1) c 1) = [c]
showExpr (Var a c 1) = (if (a < 0) then (show (-a)) else show a) ++ [c]
showExpr (Var 1 c b) = c:"^" ++ (show b)
showExpr (Var (-1) c b) = c:"^" ++ (show b)
showExpr (Var a c b) = (if (a < 0) then (show (-a)) else show a) ++ (c:"^") ++ (show b)

showPoly [] = []
showPoly ((a,b,c):xs) = (if a < 0 then "-" else "")++showExpr (Var a b c) ++ showP xs
showP [] = []
showP ((a,b,c):xs) = (if a < 0 then " - " else " + ")++showExpr (Var a b c) ++ showP xs