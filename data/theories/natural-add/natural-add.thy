name: natural-add
version: 1.51
description: Natural number addition
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-dest
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-def-1.17
}

thm {
  import: def
  package: natural-add-thm-1.45
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.0
}

main {
  import: def
  import: thm
  import: sub
}
