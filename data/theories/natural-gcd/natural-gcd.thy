name: natural-gcd
version: 1.19
description: Natural number greatest common divisor
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-def-1.12
}

thm {
  import: def
  package: natural-gcd-thm-1.20
}

lcm {
  import: def
  import: thm
  package: natural-gcd-lcm-1.10
}

main {
  import: def
  import: thm
  import: lcm
}
