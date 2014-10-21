name: base
version: 1.160
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
  checksum: 2ef4bff11ab94e83afafeb63f1ae983d36d9e4c4
}

unit {
  import: bool
  package: unit-1.16
  checksum: 0b9171d4319b0b91bdc6e29817fe36a5af8e8735
}

function {
  import: bool
  package: function-1.50
  checksum: dac02c3228b08fc555da591d2bcb661ec4c22dd4
}

pair {
  import: bool
  package: pair-1.24
  checksum: 6f77be5a72a2c8b7a595569ceb187495e6125754
}

natural {
  import: bool
  import: function
  package: natural-1.95
  checksum: 5b583748d64c3446fb267d7cc946a6bf89ba6950
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.64
  checksum: fc02e9afc85d6ced9ab503fa87c0ce4de549f8ca
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.55
  checksum: 218f49cf25b673ddb2a149b47038f46492f2fcd6
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.52
  checksum: 561149dcb14357cc7ba0c159e016786b34c3c5e0
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.65
  checksum: 936fba710e3b6a3ecf46fa6f0788214aa0be381e
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.93
  checksum: 710da2bacf80753f1d374289d68b4d017d5b1a85
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.56
  checksum: c4d2077331ceaedb4120f573c9a9c19359e59559
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
