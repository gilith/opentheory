name: relation
version: 1.0
description: Definitions and theorems about relations
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

transitive {
  package: relation-transitive-1.0
}

main {
  import: transitive
}
