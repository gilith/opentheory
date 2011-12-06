name: list-replicate
version: 1.28
description: The list replicate function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set
requires: list-length
requires: list-set
requires: list-nth
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.26
}

thm {
  import: def
  package: list-replicate-thm-1.28
}

main {
  import: def
  import: thm
}
