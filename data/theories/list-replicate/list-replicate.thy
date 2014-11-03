name: list-replicate
version: 1.61
description: The list replicate function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-nth
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.52
  checksum: e1ba45f99519e07c7bdb8670379d04d3a0b2c941
}

thm {
  import: def
  package: list-replicate-thm-1.61
  checksum: f16767711414ce88b2ac2439eeefb31637810f30
}

main {
  import: def
  import: thm
}
