name: natural-exp-log
version: 1.8
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
  package: natural-exp-log-def-1.11
  checksum: da02d8b50b4d09b5876ab30b278b2f1da143999c
}

thm {
  import: def
  package: natural-exp-log-thm-1.10
  checksum: 33ecd80e4aaef33deeba4a945f2a35584786720d
}

main {
  import: def
  import: thm
}
