name: relation
version: 1.0
description: Definitions and theorems about relations
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

transitive {
  package: relation-transitive-1.0
}

well-founded {
  package: relation-well-founded-1.0
}

measure {
  import: well-founded
  package: relation-measure-1.0
}

main {
  import: transitive
  import: well-founded
  import: measure
}
