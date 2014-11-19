name: haskell
version: 1.95
description: The Haskell base
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-02-26
requires: base
requires: byte
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
show: "Function"
show: "Haskell.Data.Byte" as "H"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Data.Word16" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Probability.Random" as "H"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: haskell-def-1.83
  checksum: ee729aa95c7f275cd9f2eb570e0a18af9c72b180
}

thm {
  import: def
  package: haskell-thm-1.63
  checksum: fe3253a55a2e711e4851683be5975c3d4f1dd003
}

src {
  import: def
  package: haskell-src-1.65
  checksum: 66971f023339bf4738991cf9c28e01eeb8e0fb66
}

test {
  import: def
  package: haskell-test-1.45
  checksum: 5ead9b7b717c13ba3b827b781e9529f0ca0ac528
}

main {
  import: def
  import: thm
  import: src
  import: test
}
