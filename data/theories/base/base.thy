name: base
version: 1.210
description: The standard theory library
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
homepage: http://opentheory.gilith.com/?pkg=base
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
haskell-test-file: haskell-test.art
haskell-equality-type: "Data.List.list"
haskell-equality-type: "Data.Option.option"
haskell-equality-type: "Data.Pair.*"
haskell-equality-type: "Data.Sum.+"
haskell-equality-type: "Number.Natural.natural"
haskell-arbitrary-type: "Data.List.list"
haskell-arbitrary-type: "Data.Option.option"
haskell-arbitrary-type: "Data.Pair.*"
haskell-arbitrary-type: "Data.Sum.+"
haskell-arbitrary-type: "Number.Natural.natural"

bool {
  package: bool-1.37
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
  import: function
  package: pair-1.29
}

natural {
  import: bool
  import: function
  package: natural-1.107
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.80
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.62
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.61
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.72
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.105
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
