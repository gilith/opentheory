name: list-filter
version: 1.51
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
  package: list-filter-def-1.46
  checksum: e13905581d4788f5079472cea913bccc2b9e96d4
}

thm {
  import: def
  package: list-filter-thm-1.52
  checksum: 28d82b08afe0b5234a25b83b19f10f66411d9407
}

main {
  import: def
  import: thm
}
