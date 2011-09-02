name: list-dest
version: 1.12
description: Theory of the list destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-dest-def-1.14
}

thm {
  import: def
  package: list-dest-thm-1.4
}

main {
  import: def
  import: thm
}
