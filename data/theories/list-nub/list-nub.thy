name: list-nub
version: 1.25
description: Definitions and theorems about the list nub function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-nub-def-1.21
}

thm {
  import: def
  package: list-nub-thm-1.26
}

main {
  import: def
  import: thm
}
