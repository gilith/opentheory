name: haskell-src
version: 1.63
description: Source of the Haskell base
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-10-22
requires: base
requires: byte
requires: haskell-def
requires: natural-bits
requires: natural-divides
requires: natural-fibonacci
requires: probability
requires: stream
requires: word16
show: "Data.Bool"
show: "Data.Byte"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Stream"
show: "Data.Word16"
show: "Haskell.Data.Byte" as "H"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Probability.Random" as "H"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Data.Word16" as "H"
show: "Number.Natural"
show: "Probability.Random"

main {
  article: "haskell-src.art"
}
