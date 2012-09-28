name: natural-exp-log
version: 1.3
description: Natural number logarithm
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-div
requires: natural-exp-def
requires: natural-exp-thm
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-log-def-1.8
}

thm {
  import: def
  package: natural-exp-log-thm-1.5
}

main {
  import: def
  import: thm
}
