name: pair
version: 1.10
description: Product types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.9
}

thm {
  import: def
  package: pair-thm-1.11
}

main {
  import: def
  import: thm
}
