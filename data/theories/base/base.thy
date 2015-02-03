name: base
version: 1.174
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
  checksum: e32a8773b452a57dd074d284f71d77234e1f9fbf
}

unit {
  import: bool
  package: unit-1.20
  checksum: 5b2b994769e08238aa28f615334e67c6b1088a83
}

function {
  import: bool
  package: function-1.55
  checksum: 35d4e6c28baaee5cee8c39751d651af7d7cfe5dc
}

pair {
  import: bool
  package: pair-1.27
  checksum: f8df648f6ee40dfb722100bee9a9c8d10b64f078
}

natural {
  import: bool
  import: function
  package: natural-1.102
  checksum: 33de6bc92a8edc786e08a89f9c2273fde00a7817
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.71
  checksum: d24337873baced38bef16c4ddb5a8c8593202453
}

relation {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: relation-1.59
  checksum: 887c60d783b8861514b89fd1671807063cbaf131
}

sum {
  import: bool
  import: pair
  import: natural
  package: sum-1.59
  checksum: 18095ea5fe7725e0df71ed5a3496c5b0910b91b4
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.71
  checksum: ce900f8898c12b26c8e92ef737fde9304c610dfb
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.101
  checksum: 174591571afbec1347ef07d570492e9f9347c2ad
}

real {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: real-1.61
  checksum: 4eb38ef27f905a03c025066f45722bb05fc6eda9
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
