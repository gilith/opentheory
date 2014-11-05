name: list-length
version: 1.51
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
  package: list-length-thm-1.41
  checksum: 2e5d3db54c129b71d861593d84e9399acda026e0
}

main {
  import: def
  import: thm
}
