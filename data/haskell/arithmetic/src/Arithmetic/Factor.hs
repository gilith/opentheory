{- |
module: Arithmetic.Factor
description: Factorized natural numbers
license: MIT

maintainer: Joe Leslie-Hurd <joe@gilith.com>
stability: provisional
portability: portable
-}
module Arithmetic.Factor
where

import qualified Data.Map as Map
import OpenTheory.Primitive.Natural

--import Arithmetic.Prime

newtype Factorized =
    Factorized {unFactorized :: Map.Map Natural Natural}
