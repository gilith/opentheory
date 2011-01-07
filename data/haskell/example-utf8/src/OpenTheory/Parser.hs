{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT
License-file: LICENSE

Maintainer: Joe Hurd
-}
module OpenTheory.Parser
  ( Token(..),
    fromList,
    toList,
    Parser(..),
    parse )
where

data Token a =
    Error
  | Eof
  | Token a (Token a)

fromList :: [a] -> Token a
fromList [] = Eof
fromList (a : t) = Token a (fromList t)

toList :: Token a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Token a t) =
    case toList t of
      Just t' -> Just (a : t')
      Nothing -> Nothing

newtype Parser a b = Parser { unParser :: (a, Token a) -> Maybe (b, Token a) }

parse :: Parser a b -> Token a -> Token b
parse _ Error = Error
parse _ Eof = Eof
parse p (Token a t) =
    case unParser p (a,t) of
      Nothing -> Error
      Just (b,t') -> Token b (parse p t')
