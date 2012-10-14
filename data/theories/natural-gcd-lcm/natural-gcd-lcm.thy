name: natural-gcd-lcm
version: 1.32
description: Natural number least common multiple
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
requires: natural-gcd-def
requires: natural-gcd-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-lcm-def-1.29
}

thm {
  import: def
  package: natural-gcd-lcm-thm-1.33
}

main {
  import: def
  import: thm
}
