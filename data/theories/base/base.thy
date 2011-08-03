name: base
version: 1.22
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
show: "Number.Real" as "Real"
show: "Relation"
show: "Set"

bool {
  package: bool-1.5
}

unit {
  import: bool
  package: unit-1.2
}

function {
  import: bool
  package: function-1.8
}

pair {
  import: bool
  package: pair-1.3
}

natural {
  import: bool
  import: function
  package: natural-1.14
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.11
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.11
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.9
}

option {
  import: bool
  import: natural
  package: option-1.16
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.14
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.8
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
