name: list-length
version: 1.14
description: Definitions and theorems about the list length function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-length-def-1.13
}

thm {
  import: def
  package: list-length-thm-1.14
}

main {
  import: def
  import: thm
}
