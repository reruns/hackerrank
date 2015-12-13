module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where
import Prelude hiding (return, fail)

class Parse a where
  parse :: Parser a
  fromString :: String -> a
  fromString cs =
    case parse cs of
            Just (s, []) -> s
            Just (s, cs) -> error ("garbage '"++cs++"'")
            Nothing -> error "Nothing"
  toString :: a -> String

type Parser a = String -> Maybe (a, String)

char :: Parser Char
char (c:cs) = Just (c,cs)
char []     = Nothing

return :: a -> Parser a
return a cs = Just (a, cs)

fail :: Parser a
fail cs = Nothing

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
