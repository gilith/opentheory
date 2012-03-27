name: natural-sub
version: 1.22
description: Natural number subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-dest
requires: natural-mult
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-sub-def-1.19
}

thm {
  import: def
  package: natural-sub-thm-1.20
}

main {
  import: def
  import: thm
}
