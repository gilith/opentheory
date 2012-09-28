{- |
module: $Header$
description: Simple stream parsers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module OpenTheory.Parser
where

import qualified OpenTheory.Parser.Stream as Stream

newtype Parser a b =
  Parser { unParser :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a) }

partialMap :: (b -> Maybe c) -> Parser a b -> Parser a c
partialMap f p =
  Parser pf
  where
  {-pf :: a -> Stream.Stream a -> Maybe (c, Stream.Stream a)-}
    pf a s =
      case unParser p a s of
        Nothing -> Nothing
        Just (b, s') ->
          case f b of
            Nothing -> Nothing
            Just c -> Just (c, s')

map :: (b -> c) -> Parser a b -> Parser a c
map f p = partialMap (\b -> Just (f b)) p

parse :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
parse _ Stream.Error = Nothing
parse _ Stream.Eof = Nothing
parse p (Stream.Cons a s) = unParser p a s

parseAll :: Parser a a
parseAll =
  Parser pa
  where
  {-pa :: a -> Stream.Stream a -> Maybe (a, Stream.Stream a)-}
    pa a s = Just (a, s)

parseNone :: Parser a b
parseNone =
  Parser pn
  where
  {-pn :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    pn _ _ = Nothing

parseOption :: (a -> Maybe b) -> Parser a b
parseOption f = partialMap f parseAll

parsePair :: Parser a b -> Parser a c -> Parser a (b, c)
parsePair pb pc =
  Parser pbc
  where
  {-pbc :: a -> Stream.Stream a -> Maybe ((b, c), Stream.Stream a)-}
    pbc a s =
      case unParser pb a s of
        Nothing -> Nothing
        Just (b, s') ->
          case parse pc s' of
            Nothing -> Nothing
            Just (c, s'') -> Just ((b, c), s'')

parseSome :: (a -> Bool) -> Parser a a
parseSome p = parseOption (\a -> if p a then Just a else Nothing)

parseStream :: Parser a b -> Stream.Stream a -> Stream.Stream b
parseStream _ Stream.Error = Stream.Error
parseStream _ Stream.Eof = Stream.Eof
parseStream p (Stream.Cons a s) =
  case unParser p a s of
    Nothing -> Stream.Error
    Just (b, s') -> Stream.Cons b (parseStream p s')
