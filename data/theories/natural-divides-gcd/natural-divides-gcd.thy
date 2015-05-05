name: natural-divides-gcd
version: 1.1
description: Natural number greatest common divisor
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-divides-def
requires: natural-divides-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-gcd-def-1.1
}

thm {
  import: def
  package: natural-divides-gcd-thm-1.1
}

main {
  import: def
  import: thm
}
