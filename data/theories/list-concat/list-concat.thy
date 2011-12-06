name: list-concat
version: 1.26
description: The list concat function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-append
requires: list-quant
show: "Data.Bool"
show: "Data.List"

def {
  package: list-concat-def-1.26
}

thm {
  import: def
  package: list-concat-thm-1.8
}

main {
  import: def
  import: thm
}
