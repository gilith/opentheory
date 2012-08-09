name: natural-funpow
version: 1.5
description: Function power
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-thm
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

def {
  package: natural-funpow-def-1.7
}

thm {
  import: def
  package: natural-funpow-thm-1.1
}

main {
  import: def
  import: thm
}
