name: list-dest
version: 1.22
description: List type destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-thm
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.24
}

thm {
  import: def
  package: list-dest-thm-1.8
}

main {
  import: def
  import: thm
}
