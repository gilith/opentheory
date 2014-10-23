name: gfp-div
version: 1.71
description: GF(p) field division
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-def
requires: gfp-thm
requires: gfp-witness
requires: list
requires: natural
requires: natural-divides
requires: natural-fibonacci
requires: natural-gcd
requires: natural-prime
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

def {
  package: gfp-div-def-1.62
  checksum: c85d2a30b63702bfd2c5eafe591271624aa774a1
}

thm {
  import: def
  package: gfp-div-thm-1.61
  checksum: 4e62c47db7567e9845f821bf635881f880463c63
}

gcd {
  import: def
  import: thm
  package: gfp-div-gcd-1.60
  checksum: 843475fc9a6996bedfe5989e6b750504977621b6
}

exp {
  import: def
  import: thm
  package: gfp-div-exp-1.39
  checksum: 78242c477941d9e84aee2e1f3fc817e9b7f1d2ac
}

main {
  import: def
  import: thm
  import: gcd
  import: exp
}
