name: natural-add-sub
version: 1.8
description: Natural number subtraction
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add-def
requires: natural-add-thm
requires: natural-def
requires: natural-dest
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-sub-def-1.10
  checksum: af52dccd54887aab5440c1c06b6560003587388d
}

thm {
  import: def
  package: natural-add-sub-thm-1.8
  checksum: 1b078ad09c7cd1ccd023338accae550632e05ffd
}

main {
  import: def
  import: thm
}
