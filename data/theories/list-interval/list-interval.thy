name: list-interval
version: 1.61
description: The list interval function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-length
requires: list-map
requires: list-nth
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.59
  checksum: a531b872597d958b6ceeac84c6ab242c7b1bac51
}

thm {
  import: def
  package: list-interval-thm-1.60
  checksum: 7723cdb05e4c53785589e6183be9f2748d5710ec
}

main {
  import: def
  import: thm
}
