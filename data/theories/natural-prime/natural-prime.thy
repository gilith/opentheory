name: natural-prime
version: 1.15
description: Prime natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
requires: natural-gcd
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-prime-def-1.14
}

thm {
  import: def
  package: natural-prime-thm-1.19
}

main {
  import: def
  import: thm
}
