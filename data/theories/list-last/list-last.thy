name: list-last
version: 1.2
description: Definitions and theorems about the list last function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.2
}

thm {
  import: def
  package: list-last-thm-1.2
}

main {
  import: def
  import: thm
}
