name: haskell-parser-test
version: 1.8
description: QuickCheck tests for simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-02-27
requires: base
requires: parser
requires: haskell
requires: haskell-parser-def
requires: haskell-parser-thm
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Haskell.Parser"
show: "Number.Natural"

main {
  article: "haskell-parser-test.art"
}
