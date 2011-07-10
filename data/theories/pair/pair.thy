name: pair
version: 1.1
description: Basic theory of product types
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.0
}

thm {
  import: def
  package: pair-thm-1.1
}

main {
  import: def
  import: thm
}
