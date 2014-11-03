name: list-reverse
version: 1.48
description: The list reverse function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-reverse-def-1.47
  checksum: 1d6b682dff9071ecdc6c9be8b6ce7ece96c2a7da
}

thm {
  import: def
  package: list-reverse-thm-1.19
  checksum: f1e9e604c0e4f66f37f9ce66c3ae2a9112b17b82
}

main {
  import: def
  import: thm
}
