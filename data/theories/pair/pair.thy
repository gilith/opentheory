name: pair
version: 1.29
description: Product types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.24
}

thm {
  import: def
  package: pair-thm-1.30
}

main {
  import: def
  import: thm
}
