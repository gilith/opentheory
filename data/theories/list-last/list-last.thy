name: list-last
version: 1.51
description: The last list function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.46
  checksum: 114e851917e379dcc9df012d7e8140ef3ff0f62d
}

thm {
  import: def
  package: list-last-thm-1.40
  checksum: 0cfd933fb08d599e25b803c64b380e8894f15a64
}

main {
  import: def
  import: thm
}
