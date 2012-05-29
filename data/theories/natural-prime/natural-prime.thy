name: natural-prime
version: 1.25
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
  package: natural-prime-def-1.23
}

thm {
  import: def
  package: natural-prime-thm-1.29
}

main {
  import: def
  import: thm
}
