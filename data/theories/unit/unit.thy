name: unit
version: 1.3
description: Basic theory of the unit type
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.3
}

thm {
  import: def
  package: unit-thm-1.2
}

main {
  import: def
  import: thm
}
