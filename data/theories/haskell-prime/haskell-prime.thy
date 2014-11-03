name: haskell-prime
version: 1.33
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
  package: haskell-prime-def-1.15
  checksum: 2976ec5203b6773f7e228dc76f1e65d8cafcff99
}

src {
  import: def
  package: haskell-prime-src-1.29
  checksum: dde96c422c5ae5bb91b33fd89eac7c6a64495898
}

test {
  import: def
  package: haskell-prime-test-1.33
  checksum: 29e7b21a5244d1082c6089ff6f283151f1e4c417
}

main {
  import: def
  import: src
  import: test
}
