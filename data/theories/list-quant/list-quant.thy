name: list-quant
version: 1.10
description: Definitions and theorems about list quantifiers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"

def {
  package: list-quant-def-1.11
}

thm {
  import: def
  package: list-quant-thm-1.10
}

main {
  import: def
  import: thm
}
