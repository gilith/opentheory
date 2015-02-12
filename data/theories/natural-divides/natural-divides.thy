name: natural-divides
version: 1.49
description: The divides relation on natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Number.Natural"
haskell-name: opentheory-divides
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: natural-divides-def-1.40
}

thm {
  import: def
  package: natural-divides-thm-1.50
}

main {
  import: def
  import: thm
}
