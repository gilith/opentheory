{- |
Module: $Header$
Description: A verified UTF8 parser
License: MIT
License-file: LICENSE

Maintainer: Joe Hurd
-}
module OpenTheory.Parser
  ( Parser(..) )
where

newtype Parser a b = Parser { unParser :: [a] -> Maybe (b,[a]) }
