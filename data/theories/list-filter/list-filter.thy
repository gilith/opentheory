name: list-filter
version: 1.21
description: Definitions and theorems about the list filter function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"

def {
  package: list-filter-def-1.18
}

thm {
  import: def
  package: list-filter-thm-1.23
}

main {
  import: def
  import: thm
}
