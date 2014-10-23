name: list-reverse
version: 1.46
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
  package: list-reverse-def-1.45
  checksum: 7b7f2b3bd5590913063f4ed2746162858b335d5e
}

thm {
  import: def
  package: list-reverse-thm-1.17
  checksum: 042d72c43e4ffc63ff0e0f0c513c3a6b68d314e9
}

main {
  import: def
  import: thm
}
