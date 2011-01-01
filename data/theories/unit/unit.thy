name: unit
version: 1.0
description: Basic theory of the unit type
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Unit"

unit-def {
  package: unit-def-1.0
}

unit-thm {
  import: unit-def
  package: unit-thm-1.0
}

main {
  import: unit-def
  import: unit-thm
}
