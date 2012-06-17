name: haskell-parser-test
version: 1.19
description: QuickCheck tests for simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-06-17
requires: base
requires: haskell
requires: haskell-parser-def
requires: haskell-parser-thm
requires: parser
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Haskell.Parser"
show: "Number.Natural"

main {
  article: "haskell-parser-test.art"
}
