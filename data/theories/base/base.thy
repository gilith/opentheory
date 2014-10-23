name: base
version: 1.162
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
  package: bool-1.33
  checksum: 8e90d684f12d754ac62d7d6792e629e4b34666d0
}

unit {
  import: bool
  package: unit-1.18
  checksum: 04580e1eb846efe713d9b36aa46c56df05cc815f
}

function {
  import: bool
  package: function-1.51
  checksum: acedcfba50acfdafabc1eae7226cbb12f369ac6e
}

pair {
  import: bool
  package: pair-1.25
  checksum: fb5669fd0f6ceda21875409d97a00b5e3ee6fe41
}

natural {
  import: bool
  import: function
  package: natural-1.97
  checksum: 3e3b100e45fd6fff7b5b27ceeaa543c1f345fcf6
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.66
  checksum: 4f2c72b0856af0fed7e4d7e4bf1720701f7cc648
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.56
  checksum: 4ddd58b58368916ac4afca45a8b3b9e362af4fab
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.53
  checksum: 02718c365cf68761bc062e2d8e744205e46565d8
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.67
  checksum: 4e287c0d93bfaa8ffb209e6c23eabcf830ae91e0
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.95
  checksum: 3a282ab898b102f43d8ba356f105d4f235ee90d6
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.58
  checksum: 7fc923734303f15302325772908739b21b0adf0b
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
