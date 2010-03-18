name: unit
version: 1.0
description: Definition and theorems about the unit type
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Unit"

require unit-def {
  package: unit-def-1.0
}

require unit-thm {
  import: unit-def
  package: unit-thm-1.0
}

theory {
  import unit-def;
  import unit-thm;
}
