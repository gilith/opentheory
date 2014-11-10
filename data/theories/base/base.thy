name: base
version: 1.167
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
  package: function-1.54
  checksum: 3d0e53f12ce723733d48ef0fb5c8b93eee22d1ac
}

pair {
  import: bool
  package: pair-1.27
  checksum: f8df648f6ee40dfb722100bee9a9c8d10b64f078
}

natural {
  import: bool
  import: function
  package: natural-1.100
  checksum: 2b47244652012ec91797e0100128d6597bcbde59
}

set {
  import: bool
  import: function
  import: pair
  import: natural
  package: set-1.69
  checksum: a85efa494254af9077aacbd76fddccaf6939acf0
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
  package: sum-1.58
  checksum: 9c66c8a4b210a82f404fb108990b94b74be0a553
}

option {
  import: bool
  import: function
  import: natural
  package: option-1.70
  checksum: 0112985521bfdc30803733a96ba5de08c96f330d
}

list {
  import: bool
  import: function
  import: pair
  import: natural
  import: set
  package: list-1.99
  checksum: a2e9043d51539df9825f50729b9bd3f0316d9005
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
