name: haskell-parser-src
version: 1.75
description: Haskell source for simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-08-15
requires: base
requires: haskell
requires: haskell-parser-def
requires: parser
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Haskell.Data.List" as "H"
show: "Haskell.Parser" as "H"
show: "Number.Natural"
show: "Parser"
show: "Probability.Random"

main {
  article: "haskell-parser-src.art"
}
