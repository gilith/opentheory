name: list-set
version: 1.5
description: Viewing lists as finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-set-def-1.5
}

thm {
  import: def
  package: list-set-thm-1.5
}

main {
  import: def
  import: thm
}
