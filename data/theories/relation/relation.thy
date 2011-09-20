name: relation
version: 1.12
description: Basic theory of relations
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

transitive {
  package: relation-transitive-1.6
}

well-founded {
  import: transitive
  package: relation-well-founded-1.11
}

measure {
  import: well-founded
  package: relation-measure-1.7
}

main {
  import: transitive
  import: well-founded
  import: measure
}
