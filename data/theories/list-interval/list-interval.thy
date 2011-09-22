name: list-interval
version: 1.13
description: Definitions and theorems about the list interval function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.15
}

thm {
  import: def
  package: list-interval-thm-1.10
}

main {
  import: def
  import: thm
}
