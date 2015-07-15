name: natural-divides-gcd
version: 1.7
description: Natural number greatest common divisor
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-divides-def
requires: natural-divides-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

def {
  package: natural-divides-gcd-def-1.3
}

thm {
  import: def
  package: natural-divides-gcd-thm-1.7
}

main {
  import: def
  import: thm
}
