name: natural-exp
version: 1.52
description: Natural number exponentiation
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-div
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-exp-def-1.35
}

thm {
  import: def
  package: natural-exp-thm-1.46
}

log {
  import: def
  import: thm
  package: natural-exp-log-1.10
}

main {
  import: def
  import: thm
  import: log
}
