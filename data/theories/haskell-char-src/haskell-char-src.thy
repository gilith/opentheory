name: haskell-char-src
version: 1.23
description: Haskell source for Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-08-08
requires: base
requires: char
requires: haskell
requires: haskell-char-def
requires: haskell-parser
requires: parser
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word16"
show: "Function"
show: "Haskell.Data.Unicode" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Parser" as "H"
show: "Number.Natural"
show: "Parser"
show: "Probability.Random"

main {
  article: "haskell-char-src.art"
}
