name: real
version: 1.58
description: The real numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.69
  checksum: 42993d55bb0ca5b69717af03ab1b360f34535f19
}

thm {
  import: def
  package: real-thm-1.46
  checksum: f7890f4bee4d2afc529e37f20598b17b2ff006b5
}

main {
  import: def
  import: thm
}
