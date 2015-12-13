module Parser(module CoreParser, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import CoreParser
infixl 7 -#, #-

type T a = Parser a

--Take the first or second of a pair
(-#) :: Parser a -> Parser b -> Parser b
(m -# n) cs = ((m # n) >-> snd) cs

(#-) :: Parser a -> Parser b -> Parser a
(m #- n) cs = ((m # n) >-> fst) cs

digit :: Parser Char
digit = (char ? isDigit)

digitVal :: Parser Int
digitVal = digit >-> digitToInt

letter :: Parser Char
letter = (char ? isAlpha)

err :: String -> Parser a
err message cs = error (message ++ " near " ++ cs ++ "\n")
