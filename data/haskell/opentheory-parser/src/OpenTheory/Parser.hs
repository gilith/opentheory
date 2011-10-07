{- |
Module: $Header$
Description: Simple stream parsers
License: MIT

Maintainer: Joe Hurd <joe@gilith.com>
Stability: provisional
Portability: portable
-}
module OpenTheory.Parser
where

import qualified OpenTheory.Parser.Stream as Stream

newtype Parser a b =
  Parser { unParser :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a) }

parse :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
parse _ Stream.Error = Nothing
parse _ Stream.Eof = Nothing
parse p (Stream.Stream a s) = unParser p a s

parseAll :: Parser a a
parseAll =
  Parser pa
  where
    pa :: a -> Stream.Stream a -> Maybe (a, Stream.Stream a)
    _ a s = Just (a,s)

parsePartialMap :: (b -> Maybe c) -> Parser a b -> Parser a c
parsePartialMap _ _ =
  Parser pf
  where
    pf :: a -> Stream.Stream a -> Maybe (c, Stream.Stream a)
    _ a s =
      case unParser p a s of
        Nothing -> Nothing
        Just (b,s') ->
          case f b of
            Nothing -> Nothing
            Just c -> Just (c,s')

parseMap :: (b -> c) -> Parser a b -> Parser a c
parseMap f p = parsePartialMap (\b -> Just (f b)) p

parseMaybe :: (a -> Maybe b) -> Parser a b
parseMaybe f = parsePartialMap f parseAll

parseNone :: Parser a b
parseNone =
  Parser pn
  where
    pn :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)
    _ _ _ = Nothing

parsePair :: Parser a b -> Parser a c -> Parser a (b, c)
parsePair _ _ =
  Parser pbc
  where
    pbc :: a -> Stream.Stream a -> Maybe ((b, c), Stream.Stream a)
    _ a s =
      case unParser pb a s of
        Nothing -> Nothing
        Just (b,s') ->
          case parse pc s' of
            Nothing -> Nothing
            Just (c,s'') -> Just ((b,c),s'')

parseSome :: (a -> Bool) -> Parser a a
parseSome p = parseMaybe (\a -> if p a then Just a else Nothing)

parseStream :: Parser a b -> Stream.Stream a -> Stream.Stream b
parseStream _ Stream.Error = Stream.Error
parseStream _ Stream.Eof = Stream.Eof
parseStream p (Stream.Stream a s) =
  case unParser p a s of
    Nothing -> Stream.Error
    Just (b,s') -> Stream.Stream b (parseStream p s')
