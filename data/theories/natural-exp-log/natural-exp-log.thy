name: natural-exp-log
version: 1.10
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
  package: natural-exp-log-def-1.13
  checksum: 4b0771b751d95b14328530b53c43044a6b51aba0
}

thm {
  import: def
  package: natural-exp-log-thm-1.12
  checksum: 9674a0608e9c3dee442680330f83d3db4a8e8aab
}

main {
  import: def
  import: thm
}
