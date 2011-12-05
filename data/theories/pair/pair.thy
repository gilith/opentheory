name: pair
version: 1.12
description: Product types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.11
}

thm {
  import: def
  package: pair-thm-1.13
}

main {
  import: def
  import: thm
}
