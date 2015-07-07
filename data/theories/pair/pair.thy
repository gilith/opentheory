name: pair
version: 1.28
description: Product types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.24
}

thm {
  import: def
  package: pair-thm-1.29
}

main {
  import: def
  import: thm
}
