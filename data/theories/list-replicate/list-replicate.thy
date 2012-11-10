name: list-replicate
version: 1.49
description: The list replicate function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-length
requires: list-nth
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.45
}

thm {
  import: def
  package: list-replicate-thm-1.52
}

main {
  import: def
  import: thm
}
