name: list-take-drop
version: 1.63
description: The list take and drop functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: list-replicate
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-take-drop-def-1.56
  checksum: f26a31693c829c949307b3874ff5c37247a42a83
}

thm {
  import: def
  package: list-take-drop-thm-1.63
  checksum: 2794d22825a8eae64574baa98f3233940596a6ad
}

main {
  import: def
  import: thm
}
