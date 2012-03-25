name: list-last
version: 1.34
description: The last list function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.27
}

thm {
  import: def
  package: list-last-thm-1.37
}

main {
  import: def
  import: thm
}
