name: natural-prime-sieve
version: 1.31
description: The sieve of Eratosthenes
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-divides
requires: natural-prime-stream
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"

def {
  package: natural-prime-sieve-def-1.32
}

thm {
  import: def
  package: natural-prime-sieve-thm-1.31
}

main {
  import: def
  import: thm
}
