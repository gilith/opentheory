name: list-last
version: 1.46
description: The last list function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.41
}

thm {
  import: def
  package: list-last-thm-1.38
}

main {
  import: def
  import: thm
}
