name: list-dest
version: 1.46
description: List type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.50
  checksum: a1703cb4d671813ae1b7f4b04e8ddd6fd3e8118a
}

thm {
  import: def
  package: list-dest-thm-1.14
  checksum: 39da03701baf25048959296041d78f4f8fdceae9
}

main {
  import: def
  import: thm
}
