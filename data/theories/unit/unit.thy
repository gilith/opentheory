name: unit
version: 1.14
description: The unit type
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.8
}

thm {
  import: def
  package: unit-thm-1.10
}

main {
  import: def
  import: thm
}
