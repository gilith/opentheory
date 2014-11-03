name: base
version: 1.164
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
  package: bool-1.35
  checksum: 52603957fbfe8dcca42a251a3bc5bda57325538c
}

unit {
  import: bool
  package: unit-1.20
  checksum: 5b2b994769e08238aa28f615334e67c6b1088a83
}

function {
  import: bool
  package: function-1.53
  checksum: 25afeaae64dad247be18236b405a8c0a8e90c7ce
}

pair {
  import: bool
  package: pair-1.27
  checksum: f8df648f6ee40dfb722100bee9a9c8d10b64f078
}

natural {
  import: bool
  import: function
  package: natural-1.99
  checksum: 3128244ec86208a81d919b6d73cb4c018840fb1e
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.68
  checksum: 7ab38f1b713c025dc27ec369f3f8ec07ccc4696c
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.58
  checksum: 8eca653b801a207bb028a4ffc027be0f769dc9e7
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.55
  checksum: a2df9268c5a26c9c1b0d45eb82a25a8a03324732
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.69
  checksum: a13e07cfa9b62cac4aa75704b2a3ddba5a889ba7
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.97
  checksum: f814f0a4d52028c0de833593654f4adc4d746d3a
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.60
  checksum: b79692bc337198f8bb2f4a9124506e3d81eabdb2
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
