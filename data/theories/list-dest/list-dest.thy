name: list-dest
version: 1.48
description: List type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.52
  checksum: f7053470719108bdc07070455dd63c1f666560e3
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
