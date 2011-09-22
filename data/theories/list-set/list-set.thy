name: list-set
version: 1.12
description: Viewing lists as finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-set-def-1.12
}

thm {
  import: def
  package: list-set-thm-1.11
}

main {
  import: def
  import: thm
}
