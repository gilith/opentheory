name: list-length
version: 1.48
description: The list length function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-length-def-1.44
  checksum: 5e211f499aec3eb8a943ea9fe81022d885a3b922
}

thm {
  import: def
  package: list-length-thm-1.39
  checksum: 67b521daeb7d6c291fb5f8c0b1c66c6eb8d6e804
}

main {
  import: def
  import: thm
}
