name: list-nth
version: 1.58
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
  package: list-nth-def-1.52
  checksum: 827b33fdae35a2f495e4b918fef783e0e8584b85
}

thm {
  import: def
  package: list-nth-thm-1.59
  checksum: 5083adb0ed64ff0c1c3743dbdb9ce58215a6f379
}

main {
  import: def
  import: thm
}
