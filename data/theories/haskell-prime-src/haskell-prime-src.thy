name: haskell-prime-src
version: 1.15
description: Haskell source for prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-10-11
requires: base
requires: haskell
requires: haskell-prime-def
requires: natural-prime
requires: stream
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Number.Natural.Prime.Sieve" as "H"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"

main {
  article: "haskell-prime-src.art"
}
