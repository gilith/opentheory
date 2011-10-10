name: unit
version: 1.5
description: Standard theory of the unit type
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.4
}

thm {
  import: def
  package: unit-thm-1.3
}

main {
  import: def
  import: thm
}
