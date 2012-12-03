name: haskell-char-test
version: 1.31
description: QuickCheck tests for Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-12-02
requires: base
requires: char
requires: haskell
requires: haskell-char-def
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.Pair"
show: "Data.Word16"
show: "Function"
show: "Haskell.Data.Unicode" as "H"
show: "Number.Natural"
show: "Probability.Random"

main {
  article: "haskell-char-test.art"
}
