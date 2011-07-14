name: list-set
version: 1.7
description: Viewing lists as finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-set-def-1.7
}

thm {
  import: def
  package: list-set-thm-1.7
}

main {
  import: def
  import: thm
}
