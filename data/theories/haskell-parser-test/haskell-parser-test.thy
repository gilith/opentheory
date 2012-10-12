name: haskell-parser-test
version: 1.44
description: QuickCheck tests for stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-10-11
requires: base
requires: haskell
requires: haskell-parser-def
requires: parser
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Parser" as "H"
show: "Number.Natural"
show: "Parser"
show: "Probability.Random"

main {
  article: "haskell-parser-test.art"
}
