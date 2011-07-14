name: list-last
version: 1.8
description: Definitions and theorems about the list last function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.8
}

thm {
  import: def
  package: list-last-thm-1.9
}

main {
  import: def
  import: thm
}
