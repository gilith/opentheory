name: natural-divides
version: 1.66
description: The divides relation on natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
homepage: http://opentheory.gilith.com/?pkg=natural-divides
requires: base
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art
haskell-name: opentheory-divides
haskell-category: Number Theory
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-test-file: haskell-test.art

def {
  package: natural-divides-def-1.41
}

thm {
  import: def
  package: natural-divides-thm-1.52
}

gcd {
  import: def
  import: thm
  package: natural-divides-gcd-1.7
}

lcm {
  import: def
  import: thm
  import: gcd
  package: natural-divides-lcm-1.0
}

main {
  import: def
  import: thm
  import: gcd
  import: lcm
}
