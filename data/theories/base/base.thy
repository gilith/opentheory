name: base
version: 1.181
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
haskell-name: opentheory
haskell-int-file: haskell.int
haskell-src-file: haskell.art

bool {
  package: bool-1.36
}

unit {
  import: bool
  package: unit-1.20
}

function {
  import: bool
  package: function-1.55
}

pair {
  import: bool
  package: pair-1.27
}

natural {
  import: bool
  import: function
  package: natural-1.102
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.71
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.59
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.59
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.71
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.102
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.61
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
