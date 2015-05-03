name: natural-divides-lcm
version: 1.0
description: Natural number least common multiple
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-divides-def
requires: natural-divides-gcd
requires: natural-divides-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-lcm-def-1.0
}

thm {
  import: def
  package: natural-divides-lcm-thm-1.0
}

main {
  import: def
  import: thm
}
