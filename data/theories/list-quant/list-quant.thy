name: list-quant
version: 1.20
description: List quantifiers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"

def {
  package: list-quant-def-1.18
}

thm {
  import: def
  package: list-quant-thm-1.22
}

main {
  import: def
  import: thm
}
