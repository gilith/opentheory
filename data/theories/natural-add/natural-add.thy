name: natural-add
version: 1.25
description: Natural number addition
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
requires: natural-numeral
requires: natural-order
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-def-1.8
}

thm {
  import: def
  package: natural-add-thm-1.17
}

main {
  import: def
  import: thm
}
