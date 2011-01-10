{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable

A verified UTF8 parser
-}
module OpenTheory.Parser
  ( Stream(..),
    fromList,
    toList,
    Parser(..),
    parse )
where

data Stream a =
    Error
  | Eof
  | Stream a (Stream a)
  deriving (Eq,Show)

fromList :: [a] -> Stream a
fromList [] = Eof
fromList (a : t) = Stream a (fromList t)

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Stream a t) =
    case toList t of
      Just t' -> Just (a : t')
      Nothing -> Nothing

newtype Parser a b =
    Parser { unParser :: a -> Stream a -> Maybe (b, Stream a) }

parse :: Parser a b -> Stream a -> Stream b
parse _ Error = Error
parse _ Eof = Eof
parse p (Stream a t) =
    case unParser p a t of
      Nothing -> Error
      Just (b,t') -> Stream b (parse p t')
