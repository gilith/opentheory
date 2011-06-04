name: list-map
version: 1.2
description: Definitions and theorems about the list map function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"

def {
  package: list-map-def-1.2
}

thm {
  import: def
  package: list-map-thm-1.2
}

main {
  import: def
  import: thm
}
