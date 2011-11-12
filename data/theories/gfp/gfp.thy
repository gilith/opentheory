name: gfp
version: 1.1
description: GF(p)
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"

def {
  package: gfp-def-1.0
}

thm {
  import: def
  package: gfp-thm-1.3
}

inverse {
  import: def
  import: thm
  package: gfp-inverse-1.1
}

main {
  import: def
  import: thm
  import: inverse
}
