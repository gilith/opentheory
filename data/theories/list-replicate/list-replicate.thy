name: list-replicate
version: 1.13
description: Definitions and theorems about the list replicate function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-replicate-def-1.14
}

thm {
  import: def
  package: list-replicate-thm-1.12
}

main {
  import: def
  import: thm
}
