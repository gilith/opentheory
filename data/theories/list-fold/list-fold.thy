name: list-fold
version: 1.25
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
  package: list-fold-def-1.24
  checksum: 1e6e1503ba98403b587edf8790ccae7c5508b20f
}

thm {
  import: def
  package: list-fold-thm-1.25
  checksum: 11851d731f71e513783cb255949ef103e39dddc0
}

main {
  import: def
  import: thm
}
