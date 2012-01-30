name: list-take-drop
version: 1.31
description: The list take and drop functions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list-def
requires: list-thm
requires: list-dest
requires: list-length
requires: list-append
requires: list-nth
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-take-drop-def-1.31
}

thm {
  import: def
  package: list-take-drop-thm-1.32
}

main {
  import: def
  import: thm
}
