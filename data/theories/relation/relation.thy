name: relation
version: 1.18
description: Basic theory of relations
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

transitive {
  package: relation-transitive-1.13
}

well-founded {
  import: transitive
  package: relation-well-founded-1.17
}

measure {
  import: well-founded
  package: relation-measure-1.11
}

main {
  import: transitive
  import: well-founded
  import: measure
}
