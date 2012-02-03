name: base
version: 1.76
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
  package: bool-1.23
}

unit {
  import: bool
  package: unit-1.13
}

function {
  import: bool
  package: function-1.29
}

pair {
  import: bool
  package: pair-1.14
}

natural {
  import: bool
  import: function
  package: natural-1.44
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.32
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.31
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.31
}

option {
  import: bool
  import: natural
  package: option-1.40
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.47
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.30
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
