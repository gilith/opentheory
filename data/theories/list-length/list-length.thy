name: list-length
version: 1.53
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
  package: list-length-def-1.47
  checksum: caa3601213368924beca9a89f305cadb705aafad
}

thm {
  import: def
  package: list-length-thm-1.43
  checksum: 03ba104ff9b948a94dbdb2b93d17233aa2134df7
}

main {
  import: def
  import: thm
}
