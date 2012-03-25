name: natural-prime
version: 1.19
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
  package: natural-prime-def-1.18
}

thm {
  import: def
  package: natural-prime-thm-1.23
}

main {
  import: def
  import: thm
}
