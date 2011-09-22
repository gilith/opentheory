name: list-append
version: 1.12
description: Definitions and theorems about the list append function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-append-def-1.11
}

thm {
  import: def
  package: list-append-thm-1.7
}

main {
  import: def
  import: thm
}
