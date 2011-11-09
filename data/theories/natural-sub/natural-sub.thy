name: natural-sub
version: 1.11
description: Natural number subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-dest
requires: natural-order
requires: natural-add
requires: natural-mult
requires: natural-even-odd
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-sub-def-1.9
}

thm {
  import: def
  package: natural-sub-thm-1.9
}

main {
  import: def
  import: thm
}
