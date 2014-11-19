name: list-filter
version: 1.55
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
  package: list-filter-def-1.49
  checksum: 70424e8f374cede0700e1b6b36fd0f3edff73d75
}

thm {
  import: def
  package: list-filter-thm-1.55
  checksum: 60bbdab1218139b3e12b93a9f13adb2f62028339
}

main {
  import: def
  import: thm
}
