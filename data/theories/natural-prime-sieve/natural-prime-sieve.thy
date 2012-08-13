name: natural-prime-sieve
version: 1.12
description: The sieve of Eratosthenes
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: natural-divides
requires: natural-prime-stream
requires: pair
requires: relation
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"

def {
  package: natural-prime-sieve-def-1.17
}

thm {
  import: def
  package: natural-prime-sieve-thm-1.15
}

main {
  import: def
  import: thm
}
