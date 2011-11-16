name: list-dest
version: 1.21
description: List type destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.23
}

thm {
  import: def
  package: list-dest-thm-1.8
}

main {
  import: def
  import: thm
}
