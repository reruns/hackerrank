import Prelude hiding (return, fail)

type Parser a = String -> [(a, String)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

return :: a => Parser a
return v = (\inp -> [(v, inp)])

fail :: a => Parser a
fail v = (\inp -> [])

item :: Parser Char
item = \inp -> case inp of
               [] -> []
               (x:xs) -> (x,xs)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else fail

char :: Char -> Parser Char
char x = sat (==x)

digit :: Parser Char
digit = sat isDigit

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                  [] -> []
                  [(v,out)] -> parse (f v) out

(+++) :: Parser a -> Parser a -> Parser a
p +++ q :: \inp -> case parse p inp of
                   [] -> parse q inp
                   [(v,out)] -> [(v,out)]
