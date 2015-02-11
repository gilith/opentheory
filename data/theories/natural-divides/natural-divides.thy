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
  checksum: f36539e3dff9b7d00833cedef36c927aad33e33f
}

thm {
  import: def
  package: natural-divides-thm-1.50
  checksum: 4434d32c99b6bbe1ba1d514844ca6c7c8c592b53
}

main {
  import: def
  import: thm
}
