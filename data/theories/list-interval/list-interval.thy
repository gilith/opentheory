name: list-interval
version: 1.60
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
  package: list-interval-def-1.58
  checksum: 3bb9c7501947db351a7e541e113263a55cc78e99
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
