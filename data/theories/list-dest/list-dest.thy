name: list-dest
version: 1.41
description: List type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.45
}

thm {
  import: def
  package: list-dest-thm-1.13
}

main {
  import: def
  import: thm
}
