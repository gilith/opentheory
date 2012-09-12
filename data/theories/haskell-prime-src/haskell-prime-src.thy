name: haskell-prime-src
version: 1.9
description: Haskell source for prime numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-08-13
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
