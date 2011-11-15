name: gfp-inverse
version: 1.5
description: GF(p) inverse
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"

def {
  package: gfp-inverse-def-1.5
}

thm {
  import: def
  package: gfp-inverse-thm-1.3
}

main {
  import: def
  import: thm
}
