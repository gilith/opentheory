name: group-mult-add
version: 1.3
description: Group multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: group-mult-def
requires: group-mult-thm
requires: group-thm
requires: group-witness
requires: list
requires: natural
requires: natural-bits
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: group-mult-add-def-1.7
}

thm {
  import: def
  package: group-mult-add-thm-1.6
}

main {
  import: def
  import: thm
}
