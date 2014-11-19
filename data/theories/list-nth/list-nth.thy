name: list-nth
version: 1.62
description: The list nth function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-last
requires: list-length
requires: list-map
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nth-def-1.55
  checksum: b83b07173d8b3b723a252013c00bafbbc2a3b702
}

thm {
  import: def
  package: list-nth-thm-1.62
  checksum: 138b667c7c954983a3c8527ef07eb4fcc1fcb231
}

main {
  import: def
  import: thm
}
