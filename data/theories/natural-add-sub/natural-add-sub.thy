name: natural-add-sub
version: 1.2
description: Natural number subtraction
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add-def
requires: natural-add-thm
requires: natural-def
requires: natural-dest
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-sub-def-1.4
}

thm {
  import: def
  package: natural-add-sub-thm-1.4
}

main {
  import: def
  import: thm
}
