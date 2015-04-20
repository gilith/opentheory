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
import qualified OpenTheory.Primitive.Natural as Natural

newtype Parser a b =
  Parser { unParser :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a) }

token :: (a -> Maybe b) -> Parser a b
token f =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    prs x xs =
      case f x of
        Nothing -> Nothing
        Just y -> Just (y, xs)

some :: (a -> Bool) -> Parser a a
some p = token (\x -> if p x then Just x else Nothing)

anyToken :: Parser a a
anyToken = some (const True)

apply :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
apply _ Stream.Error = Nothing
apply _ Stream.Eof = Nothing
apply p (Stream.Cons x xs) = unParser p x xs

mapPartial :: Parser a b -> (b -> Maybe c) -> Parser a c
mapPartial p f =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (c, Stream.Stream a)-}
    prs x xs =
      case unParser p x xs of
        Nothing -> Nothing
        Just (y, ys) ->
          case f y of
            Nothing -> Nothing
            Just z -> Just (z, ys)

filterParser :: Parser a b -> (b -> Bool) -> Parser a b
filterParser p f = mapPartial p (\x -> if f x then Just x else Nothing)

fold :: (a -> c -> Maybe (Either b c)) -> c -> Parser a b
fold f =
  Parser . prs
  where
  {-prs :: c -> a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    prs s x xs =
      case f x s of
        Nothing -> Nothing
        Just y ->
          case y of
            Left z -> Just (z, xs)
            Right t ->
              case xs of
                Stream.Error -> Nothing
                Stream.Eof -> Nothing
                Stream.Cons z zs -> prs t z zs

foldN :: (a -> b -> Maybe b) -> Natural.Natural -> b -> Parser a b
foldN f n s =
  fold
    (\x (m, t) ->
       fmap (\u -> if m == 0 then Left u else Right (m - 1, u)) (f x t))
    (n, s)

mapParser :: Parser a b -> (b -> c) -> Parser a c
mapParser p f = mapPartial p (\x -> Just (f x))

none :: Parser a b
none = token (const Nothing)

orelse :: Parser a b -> Parser a b -> Parser a b
orelse p1 p2 =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    prs x xs =
      case unParser p1 x xs of
        Nothing -> unParser p2 x xs
        Just yys -> Just yys

sequenceParser :: Parser a (Parser a b) -> Parser a b
sequenceParser p =
  Parser prs
  where
  {-prs :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)-}
    prs x xs =
      case unParser p x xs of
        Nothing -> Nothing
        Just (q, ys) -> apply q ys

pair :: Parser a b -> Parser a c -> Parser a (b, c)
pair p1 p2 =
  sequenceParser (mapParser p1 (\x -> mapParser p2 (\y -> (x, y))))

parse :: Parser a b -> Stream.Stream a -> Stream.Stream b
parse _ Stream.Error = Stream.Error
parse _ Stream.Eof = Stream.Eof
parse p (Stream.Cons x xs) =
  case unParser p x xs of
    Nothing -> Stream.Error
    Just (y, ys) -> Stream.Cons y (parse p ys)
