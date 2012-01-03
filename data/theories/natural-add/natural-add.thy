name: natural-add
version: 1.33
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
  package: natural-add-def-1.12
}

thm {
  import: def
  package: natural-add-thm-1.25
}

main {
  import: def
  import: thm
}
