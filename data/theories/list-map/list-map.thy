name: list-map
version: 1.51
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
  package: list-map-def-1.45
  checksum: 7dce84e887bc3208ca14928d6013f5c853e64534
}

thm {
  import: def
  package: list-map-thm-1.56
  checksum: 7fd0b5086680a1fafb93e23812a6c68e59499f7e
}

main {
  import: def
  import: thm
}
