name: list-map
version: 1.54
description: The list map function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-set
requires: list-thm
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-map-def-1.48
  checksum: d77d7b0a2a18aef6471919aa059b4727c2ea41db
}

thm {
  import: def
  package: list-map-thm-1.58
  checksum: f6e6439dcd299d7d4eafd6c011075b2af3356a3f
}

main {
  import: def
  import: thm
}
