name: natural-prime-sieve
version: 1.25
description: The sieve of Eratosthenes
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: natural-prime-sieve-def-1.28
  checksum: e05cb8129c6e3d0bc18f6fc49dd7395816ddadda
}

thm {
  import: def
  package: natural-prime-sieve-thm-1.26
  checksum: 7fae69a8b5d48fa799f997efc17d01af6570719c
}

main {
  import: def
  import: thm
}
