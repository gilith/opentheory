name: list-take-drop
version: 1.37
description: The list take and drop functions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-take-drop-def-1.36
}

thm {
  import: def
  package: list-take-drop-thm-1.39
}

main {
  import: def
  import: thm
}
