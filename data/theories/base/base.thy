name: base
version: 1.131
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
  package: function-1.48
}

pair {
  import: bool
  package: pair-1.20
}

natural {
  import: bool
  import: function
  package: natural-1.80
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.56
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.50
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.48
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.60
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.77
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.51
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
