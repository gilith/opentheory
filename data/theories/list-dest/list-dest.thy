name: list-dest
version: 1.14
description: Theory of the list destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.16
}

thm {
  import: def
  package: list-dest-thm-1.5
}

main {
  import: def
  import: thm
}
