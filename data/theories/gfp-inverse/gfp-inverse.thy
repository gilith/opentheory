name: gfp-inverse
version: 1.8
description: GF(p) inverse
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"

def {
  package: gfp-inverse-def-1.8
}

thm {
  import: def
  package: gfp-inverse-thm-1.6
}

main {
  import: def
  import: thm
}
