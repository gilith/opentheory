name: haskell
version: 1.66
description: The Haskell base
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-02-26
requires: base
requires: byte
requires: natural-bits
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
  package: haskell-def-1.59
}

thm {
  import: def
  package: haskell-thm-1.49
}

src {
  import: def
  package: haskell-src-1.44
}

test {
  import: def
  package: haskell-test-1.29
}

main {
  import: def
  import: thm
  import: src
  import: test
}
