name: list-interval
version: 1.18
description: Definitions and theorems about the list interval function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.19
}

thm {
  import: def
  package: list-interval-thm-1.16
}

main {
  import: def
  import: thm
}
