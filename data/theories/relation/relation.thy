name: relation
version: 1.2
description: Basic theory of relations
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Pair"
show: "Number.Natural"
show: "Relation"

transitive {
  package: relation-transitive-1.2
}

well-founded {
  package: relation-well-founded-1.2
}

measure {
  import: well-founded
  package: relation-measure-1.2
}

main {
  import: transitive
  import: well-founded
  import: measure
}
