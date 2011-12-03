name: base
version: 1.68
description: The standard theory library
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: empty
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Data.Unit"
show: "Function"
show: "Number.Natural"
show: "Number.Real" as "Real"
show: "Relation"
show: "Set"

bool {
  package: bool-1.19
}

unit {
  import: bool
  package: unit-1.9
}

function {
  import: bool
  package: function-1.24
}

pair {
  import: bool
  package: pair-1.11
}

natural {
  import: bool
  import: function
  package: natural-1.38
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.28
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.27
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.27
}

option {
  import: bool
  import: natural
  package: option-1.35
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.42
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.26
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
