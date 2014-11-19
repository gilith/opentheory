name: list-dest
version: 1.50
description: List type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.54
  checksum: 193ec44f143c1d17f56d97181251c2b5b9a19b42
}

thm {
  import: def
  package: list-dest-thm-1.16
  checksum: baaba2600af423026bad99bf774a28b892403926
}

main {
  import: def
  import: thm
}
