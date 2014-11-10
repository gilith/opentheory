name: list-fold
version: 1.29
description: List fold operations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-length
requires: list-reverse
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"

def {
  package: list-fold-def-1.28
  checksum: 1471a660db60f3f31036a5cf24cc37ff2a46414a
}

thm {
  import: def
  package: list-fold-thm-1.28
  checksum: 08a9f9a00b9c1628747f68117ad5b0ef4e824558
}

main {
  import: def
  import: thm
}
