name: list-nub
version: 1.15
description: Definitions and theorems about the list nub function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-nub-def-1.15
}

thm {
  import: def
  package: list-nub-thm-1.15
}

main {
  import: def
  import: thm
}
