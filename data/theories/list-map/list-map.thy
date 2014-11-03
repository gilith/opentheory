name: list-map
version: 1.53
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
  package: list-map-def-1.47
  checksum: 3b84099d1068797a5232896a4a318ae97a4a90d6
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
