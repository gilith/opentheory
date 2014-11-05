name: list-dest
version: 1.49
description: List type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.53
  checksum: 103d4a9c481f3c19ba6147342de1b63d3aa80e07
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
