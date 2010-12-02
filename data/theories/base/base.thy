name: base
version: 1.0
description: Basic theories
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Unit"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"
show: "Relation"

bool {
  package: bool-1.0
}

unit {
  import: bool
  package: unit-1.0
}

function {
  import: bool
  package: function-1.0
}

pair {
  import: bool
  package: pair-1.0
}

natural {
  import: bool
  import: function
  package: natural-1.0
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  package: relation-1.0
}

sum {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  import: relation
  package: sum-1.0
}

option {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  import: relation
  import: sum
  package: option-1.0
}

list {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  import: relation
  import: sum
  import: option
  package: list-1.0
}

main {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  import: relation
  import: sum
  import: option
  import: list
}
