name: unit
version: 1.0
description: Definition and theorems about the unit type
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
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
