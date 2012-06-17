name: natural-add
version: 1.46
description: Natural number addition
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-def-1.16
}

thm {
  import: def
  package: natural-add-thm-1.39
}

main {
  import: def
  import: thm
}
