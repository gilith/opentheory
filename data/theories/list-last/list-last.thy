name: list-last
version: 1.25
description: The last list function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: set
requires: list-def
requires: list-thm
requires: list-set
requires: list-append
show: "Data.Bool"
show: "Data.List"
show: "Set"

def {
  package: list-last-def-1.19
}

thm {
  import: def
  package: list-last-thm-1.28
}

main {
  import: def
  import: thm
}
