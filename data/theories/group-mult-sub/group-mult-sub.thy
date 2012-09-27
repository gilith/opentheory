name: group-mult-sub
version: 1.1
description: Group multiplication by repeated subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: group-def
requires: group-mult-def
requires: group-mult-thm
requires: group-thm
requires: group-witness
requires: list
requires: natural
requires: natural-fibonacci
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: group-mult-sub-def-1.5
}

thm {
  import: def
  package: group-mult-sub-thm-1.3
}

main {
  import: def
  import: thm
}
