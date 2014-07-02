name: base
version: 1.159
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
  package: bool-1.32
}

unit {
  import: bool
  package: unit-1.16
}

function {
  import: bool
  package: function-1.50
}

pair {
  import: bool
  package: pair-1.24
}

natural {
  import: bool
  import: function
  package: natural-1.95
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.63
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.55
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.52
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.65
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.93
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.56
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
