name: list-nth
version: 1.22
description: Definitions and theorems about the list nth function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-nth-def-1.20
}

thm {
  import: def
  package: list-nth-thm-1.24
}

main {
  import: def
  import: thm
}
