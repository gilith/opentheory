name: base
version: 1.134
description: The standard theory library
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Data.Unit"
show: "Function"
show: "Number.Natural"
show: "Number.Real"
show: "Relation"
show: "Set"

bool {
  package: bool-1.29
}

unit {
  import: bool
  package: unit-1.15
}

function {
  import: bool
  package: function-1.49
}

pair {
  import: bool
  package: pair-1.22
}

natural {
  import: bool
  import: function
  package: natural-1.82
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.58
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.52
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.50
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.62
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.79
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.53
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
  import: real
}
