{- |
module: $Header$
description: Stream parsers
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

anyToken :: Parser a a
anyToken =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (a, Stream.Stream a)-}
    prs a s = Just (a, s)

apply :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
apply _ Stream.Error = Nothing
apply _ Stream.Eof = Nothing
apply p (Stream.Cons a s) = unParser p a s

mapPartial :: (b -> Maybe c) -> Parser a b -> Parser a c
mapPartial f p =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (c, Stream.Stream a)-}
    prs a s =
      case unParser p a s of
        Nothing -> Nothing
        Just (b, s') ->
          case f b of
            Nothing -> Nothing
            Just c -> Just (c, s')

mapParser :: (b -> c) -> Parser a b -> Parser a c
mapParser f p = mapPartial (\b -> Just (f b)) p

none :: Parser a b
none =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    prs _ _ = Nothing

option :: (a -> Maybe b) -> Parser a b
option f = mapPartial f anyToken

pair :: Parser a b -> Parser a c -> Parser a (b, c)
pair p0 p1 =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe ((b, c), Stream.Stream a)-}
    prs a s =
      case unParser p0 a s of
        Nothing -> Nothing
        Just (b, s') ->
          case apply p1 s' of
            Nothing -> Nothing
            Just (c, s'') -> Just ((b, c), s'')

parse :: Parser a b -> Stream.Stream a -> Stream.Stream b
parse _ Stream.Error = Stream.Error
parse _ Stream.Eof = Stream.Eof
parse p (Stream.Cons a s) =
  case unParser p a s of
    Nothing -> Stream.Error
    Just (b, s') -> Stream.Cons b (parse p s')

some :: (a -> Bool) -> Parser a a
some p = option (\a -> if p a then Just a else Nothing)
