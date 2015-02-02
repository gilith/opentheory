name: list-length
version: 1.52
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
  package: list-length-thm-1.42
  checksum: 6b359afbb6802bb767175a11385027ce26f42dc5
}

main {
  import: def
  import: thm
}
