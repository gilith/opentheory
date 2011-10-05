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

import qualified OpenTheory.Parser.parseAll as parseAll
import qualified OpenTheory.Parser.parseNone as parseNone
import qualified OpenTheory.Parser.parsePair as parsePair
import qualified OpenTheory.Parser.parsePartialMap as parsePartialMap
import qualified OpenTheory.Parser.Stream as Stream

newtype Parser a b =
  Parser { unParser :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a) }

parse :: Parser a b -> Stream.Stream a -> Maybe (b, Stream.Stream a)
parse _ Stream.Error = Nothing
parse _ Stream.Eof = Nothing
parse p (Stream.Stream a s) = unParser p a s

parseAll :: Parser a a
parseAll =
  Parser parseAll.pa
  where
    parseAll.pa :: a -> Stream.Stream a -> Maybe (a, Stream.Stream a)
    parseAll.pa a s = Just (a,s)

parsePartialMap :: (b -> Maybe c) -> Parser a b -> Parser a c
parsePartialMap f p =
  Parser (parsePartialMap.pf f p)
  where
    parsePartialMap.pf ::
      (b -> Maybe c) -> Parser a b -> a -> Stream.Stream a ->
        Maybe (c, Stream.Stream a)
    parsePartialMap.pf f p a s =
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
  Parser parseNone.pn
  where
    parseNone.pn :: a -> Stream.Stream a -> Maybe (b, Stream.Stream a)
    parseNone.pn _ _ = Nothing

parsePair :: Parser a b -> Parser a c -> Parser a (b, c)
parsePair pb pc =
  Parser (parsePair.pbc pb pc)
  where
    parsePair.pbc ::
      Parser a b -> Parser a c -> a -> Stream.Stream a ->
        Maybe ((b, c), Stream.Stream a)
    parsePair.pbc pb pc a s =
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
