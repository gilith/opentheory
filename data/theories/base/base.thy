name: base
version: 1.8
description: The standard theory library
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Data.Unit"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"
show: "Relation"

bool {
  package: bool-1.2
}

unit {
  import: bool
  package: unit-1.0
}

function {
  import: bool
  package: function-1.1
}

pair {
  import: bool
  package: pair-1.0
}

natural {
  import: bool
  import: function
  package: natural-1.2
}

set {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  package: set-1.0
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  package: relation-1.2
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.2
}

option {
  import: bool
  import: natural
  package: option-1.6
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.5
}

main {
  import: bool
  import: unit
  import: function
  import: pair
  import: natural
  import: set
  import: relation
  import: sum
  import: option
  import: list
}
