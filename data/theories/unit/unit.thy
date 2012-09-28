name: unit
version: 1.15
description: The unit type
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.9
}

thm {
  import: def
  package: unit-thm-1.11
}

main {
  import: def
  import: thm
}
