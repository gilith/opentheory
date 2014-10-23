name: natural-div
version: 1.47
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
  package: natural-div-def-1.38
  checksum: 4c3a6a9c85695f749c724a2f1a73f04adadd0a85
}

thm {
  import: def
  package: natural-div-thm-1.51
  checksum: bfd5c3868bef324d83cc4baa03470c11adb9fca3
}

main {
  import: def
  import: thm
}
