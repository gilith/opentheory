name: list-replicate
version: 1.6
description: Definitions and theorems about the list replicate function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-replicate-def-1.7
}

thm {
  import: def
  package: list-replicate-thm-1.6
}

main {
  import: def
  import: thm
}
