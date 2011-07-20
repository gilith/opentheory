name: list-reverse
version: 1.10
description: Definitions and theorems about the list reverse function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-reverse-def-1.11
}

thm {
  import: def
  package: list-reverse-thm-1.3
}

main {
  import: def
  import: thm
}
