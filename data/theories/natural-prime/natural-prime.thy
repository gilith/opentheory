name: natural-prime
version: 1.81
description: Prime natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-divides
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve" as "Sieve"
haskell-name: opentheory-prime
haskell-category: Number Theory
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-test-file: haskell-test.art

def {
  package: natural-prime-def-1.44
}

thm {
  import: def
  package: natural-prime-thm-1.54
}

stream {
  import: thm
  package: natural-prime-stream-1.30
}

sieve {
  import: stream
  package: natural-prime-sieve-1.31
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
