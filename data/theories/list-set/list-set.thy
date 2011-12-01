name: list-set
version: 1.24
description: List to set conversions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set
requires: list-def
requires: list-thm
requires: list-length
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-set-def-1.23
}

thm {
  import: def
  package: list-set-thm-1.23
}

main {
  import: def
  import: thm
}
