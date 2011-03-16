name: list-replicate
version: 1.1
description: Definitions and theorems about the list replicate function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-replicate-def-1.0
}

thm {
  import: def
  package: list-replicate-thm-1.1
}

main {
  import: def
  import: thm
}
