name: list-reverse
version: 1.20
description: Definitions and theorems about the list reverse function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-reverse-def-1.19
}

thm {
  import: def
  package: list-reverse-thm-1.9
}

main {
  import: def
  import: thm
}
