name: pair
version: 1.23
description: Product types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.20
}

thm {
  import: def
  package: pair-thm-1.24
}

main {
  import: def
  import: thm
}
