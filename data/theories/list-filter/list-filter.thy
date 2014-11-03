name: list-filter
version: 1.53
description: The list filter function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-filter-def-1.48
  checksum: b167c0a690d0ef82aa5708016c3fca230fa2483b
}

thm {
  import: def
  package: list-filter-thm-1.54
  checksum: 68c9f46f92a7e41b891082b3828f6b9d1a60233b
}

main {
  import: def
  import: thm
}
