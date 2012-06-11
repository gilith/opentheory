name: pair
version: 1.19
description: Product types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.17
}

thm {
  import: def
  package: pair-thm-1.21
}

main {
  import: def
  import: thm
}
