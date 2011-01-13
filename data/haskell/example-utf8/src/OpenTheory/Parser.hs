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
    parse,
    parsePair,
    parseMaybe,
    parseSome,
    parseStream )
where

data Stream a =
    Error
  | Eof
  | Stream a (Stream a)
  deriving (Eq,Show)

fromList :: [a] -> Stream a
fromList [] = Eof
fromList (a : s) = Stream a (fromList s)

toList :: Stream a -> Maybe [a]
toList Error = Nothing
toList Eof = Just []
toList (Stream a s) =
    case toList s of
      Just s' -> Just (a : s')
      Nothing -> Nothing

newtype Parser a b =
    Parser { unParser :: a -> Stream a -> Maybe (b, Stream a) }

parse :: Parser a b -> Stream a -> Maybe (b, Stream a)
parse _ Error = Nothing
parse _ Eof = Nothing
parse p (Stream a s) = unParser p a s

parsePair :: Parser a b -> Parser a c -> Parser a (b,c)
parsePair pb pc =
    Parser pbc
  where
    pbc a s =
      case unParser pb a s of
        Nothing -> Nothing
        Just (b,s') ->
          case parse pc s' of
            Nothing -> Nothing
            Just (c,s'') -> Just ((b,c),s'')

parseMaybe :: (a -> Maybe b) -> Parser a b
parseMaybe f =
    Parser pb
  where
    pb a s =
      case f a of
        Just b -> Just (b,s)
        Nothing -> Nothing

parseSome :: (a -> Bool) -> Parser a a
parseSome p =
    parseMaybe f
  where
    f a = if p a then Just a else Nothing

parseStream :: Parser a b -> Stream a -> Stream b
parseStream _ Error = Error
parseStream _ Eof = Eof
parseStream p (Stream a s) =
    case unParser p a s of
      Nothing -> Error
      Just (b,s') -> Stream b (parseStream p s')
