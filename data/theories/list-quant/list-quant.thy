name: list-quant
version: 1.22
description: List quantifiers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: set
requires: list-def
requires: list-set
requires: list-append
requires: list-map
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Set"

def {
  package: list-quant-def-1.20
}

thm {
  import: def
  package: list-quant-thm-1.24
}

main {
  import: def
  import: thm
}
