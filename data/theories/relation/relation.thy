name: relation
version: 1.58
description: Relation operators
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
show: "Relation"
show: "Set"

def {
  package: relation-def-1.25
  checksum: 9f7a83d975e6e78994a4c5cbf2a328f27bc1d513
}

thm {
  import: def
  package: relation-thm-1.14
  checksum: 8d24a179cd39cc50a8e9182f7465ee8398dab883
}

well-founded {
  import: def
  import: thm
  package: relation-well-founded-1.54
  checksum: fa80792d8173d077c5249241fdc95ad9bd91e7e6
}

natural {
  import: def
  import: thm
  import: well-founded
  package: relation-natural-1.34
  checksum: 88fd7c28780588d69ece49b0a9a2543bcc5e5759
}

main {
  import: def
  import: thm
  import: well-founded
  import: natural
}
