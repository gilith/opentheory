name: list-interval
version: 1.3
description: Definitions and theorems about the list interval function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-interval-def-1.4
}

thm {
  import: def
  package: list-interval-thm-1.0
}

main {
  import: def
  import: thm
}
