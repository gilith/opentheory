name: gfp
version: 1.5
description: GF(p)
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"

def {
  package: gfp-def-1.1
}

thm {
  import: def
  package: gfp-thm-1.4
}

inverse {
  import: def
  import: thm
  package: gfp-inverse-1.5
}

main {
  import: def
  import: thm
  import: inverse
}
