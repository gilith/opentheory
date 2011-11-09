name: list-interval
version: 1.17
description: Definitions and theorems about the list interval function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.18
}

thm {
  import: def
  package: list-interval-thm-1.14
}

main {
  import: def
  import: thm
}
