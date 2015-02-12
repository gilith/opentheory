name: unit
version: 1.20
description: The unit type
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.13
}

thm {
  import: def
  package: unit-thm-1.16
}

main {
  import: def
  import: thm
}
