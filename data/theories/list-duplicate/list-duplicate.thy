name: list-duplicate
version: 1.0
description: Definitions and theorems about the list duplicate function
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Data.List"

def {
  package: list-duplicate-def-1.0
}

thm {
  import: def
  package: list-duplicate-thm-1.0
}

main {
  import: def
  import: thm
}
