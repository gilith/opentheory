name: list-replicate
version: 1.33
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
  package: list-replicate-def-1.32
}

thm {
  import: def
  package: list-replicate-thm-1.34
}

main {
  import: def
  import: thm
}
