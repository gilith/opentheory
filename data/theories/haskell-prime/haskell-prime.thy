name: haskell-prime
version: 1.31
description: Prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
haskell-category: Number Theory
requires: base
requires: haskell
requires: natural-divides
requires: natural-prime
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Haskell.Data.Stream" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Number.Natural.Prime.Sieve" as "H"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"
show: "Probability.Random"

def {
  package: haskell-prime-def-1.13
  checksum: c3314e90dc1bbd8642f329eb3fa008b4a9fb701d
}

src {
  import: def
  package: haskell-prime-src-1.27
  checksum: f9c25f9d594a504cf06c1b7a1248043debe0dc2a
}

test {
  import: def
  package: haskell-prime-test-1.31
  checksum: 45725e2a85e3e42ff28103d7a0cc6f81178a1845
}

main {
  import: def
  import: src
  import: test
}
