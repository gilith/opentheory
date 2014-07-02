name: haskell-prime-def
version: 1.11
description: Definition of prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-06-12
requires: base
requires: haskell
requires: natural-prime
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Number.Natural.Prime.Sieve" as "H"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"

main {
  article: "haskell-prime-def.art"
}
