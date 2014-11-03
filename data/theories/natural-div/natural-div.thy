name: natural-div
version: 1.49
description: Natural number division
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-def-1.40
  checksum: f18696bccf5013ec5ab621f9dc3db481b9a950d2
}

thm {
  import: def
  package: natural-div-thm-1.53
  checksum: 0dfdb1cb42ece034df502dc53cefd48c3e9bfe07
}

main {
  import: def
  import: thm
}
