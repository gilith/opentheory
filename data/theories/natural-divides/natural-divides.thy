name: natural-divides
version: 1.52
description: The divides relation on natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Number.Natural"
haskell-name: opentheory-divides
haskell-category: Number Theory
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: natural-divides-def-1.41
}

thm {
  import: def
  package: natural-divides-thm-1.51
}

main {
  import: def
  import: thm
}
