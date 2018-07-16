{-#OPTIONS_GHC -Wall #-}

{-# LANGUAGE LambdaCase #-}

module Combinators where

import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
import Data.Char

import Types (
  Error(..)
  )


-- | A 'Parser a' is defined by its parse function. The parse function
-- parse :: String -> [(a, String)] ensures that all parses must be 
-- constructor with a method for parsing the particular a.
newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
  fmap f (Parser x) = Parser (\s -> [(f a, b) | (a, b) <- x s])

instance Applicative Parser where
  pure                        = unit
  (<*>) (Parser x) (Parser y) = Parser (\s -> 
                         [ (f a, s2) | (f, s1) <- x s, (a, s2) <- y s1])

instance Monad Parser where
  return = pure
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = failure
  (<|>) = option

-- | The 'run'  function takes two arguments, a Parser and a String, and 
-- applies the parse function in the parser to the string. 
run :: Parser a -> String -> Either Error a
run p s = case parse p s of
  [(result, [])]   -> Right result
  [(_, cs)] -> Left (E ("Parser did not consume complete stream: " ++ cs))
  []        -> Left (E "Parser is empty somehow?")
  _         -> Left (E "Some unknown error occured!")

-- | The 'item' combinator returns a single Character Parser.
item :: Parser Char
item = Parser $ \case
  []     -> []
  (c:cs) -> [(c, cs)]

-- | The 'bind' function is a convenience function to ensure that Parsers
-- are Monads. 
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ 
  \s -> concatMap (\(a, s') -> parse (f a) s') (parse p s)

-- | The 'unit' function is a convenience function to ensure that Parsers
-- are Applicative (pure).
unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

-- | The 'combine' function takes two parsers and returns a parser that 
-- appends the parse result of each parser.
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- | The 'failure' combinator returns an empty Parser
failure :: Parser a
failure = Parser (const [])

-- | The 'option' function takes two parsers and applies the second to the
-- result of the first, if the first was successful.
option :: Parser a -> Parser a -> Parser a
option p q = Parser (\s ->
  case parse p s of
   []      -> parse q s
   result  -> result
  )

-- | The 'manyTill' function takes a parser `p` and some parser `end`. It 
-- returns a list of repeated p until end is encountered. This parser will
-- break if `p` overlaps with `end`!
manyTill :: Parser a -> Parser a -> Parser [a]
manyTill p end = ([] <$ end) <|> ((:) <$> p <*> manyTill p end)

-- | The 'between' function takes the starting and ending parser and some
-- parser p. It will attempt to parse p between start and end.
between :: Parser start -> Parser end -> Parser a -> Parser a
between start end p = start *> p <* end

-- | The 'satisfy' function takes a predicate (Char -> Bool) and returns
-- a Parser Char if that predicate is satisfied, or 'failure' otherwise.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then unit c else failure

-- | The 'oneOf' function takes a [Char] and attempts to satisfy each
-- character in order, returning the first Parser Char that succeeds.
oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

-- | The 'char' function is a convenience function that checks if parsed
-- Character is equal.
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | The 'anyChar' combinator succeeds on any character.
anyChar :: Parser Char
anyChar = satisfy $ const True

-- | The 'alphaNum' combinator succeeds on any alphanumeric char or '-'
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum <|> char '-'

----------------String specific functions----------

-- | The 'whitespace' combinator succeeds on zero or more occurances of
-- whitespace.
whitespace :: Parser String
whitespace = many (oneOf " \n\r\t")

-- | The 'string' function takes a String and returns a Parser String on a
-- successful Parse.
string :: String -> Parser String
string []       = return []
string (c : cs) = char c >>= \_ -> string cs >>= \_ -> return (c:cs)

