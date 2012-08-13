name: natural-gcd-lcm
version: 1.28
description: Natural number least common multiple
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
requires: natural-gcd-def
requires: natural-gcd-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-lcm-def-1.25
}

thm {
  import: def
  package: natural-gcd-lcm-thm-1.29
}

main {
  import: def
  import: thm
}
