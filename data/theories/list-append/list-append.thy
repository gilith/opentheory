name: list-append
version: 1.53
description: Appending lists
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-append-def-1.46
}

thm {
  import: def
  package: list-append-thm-1.26
}

main {
  import: def
  import: thm
}
