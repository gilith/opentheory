name: natural-add
version: 1.55
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
  package: natural-add-def-1.19
}

thm {
  import: def
  package: natural-add-thm-1.49
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.2
}

main {
  import: def
  import: thm
  import: sub
}
