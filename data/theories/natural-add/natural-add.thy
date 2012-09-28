name: natural-add
version: 1.52
description: Natural number addition
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: natural-add-def-1.18
}

thm {
  import: def
  package: natural-add-thm-1.46
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.1
}

main {
  import: def
  import: thm
  import: sub
}
