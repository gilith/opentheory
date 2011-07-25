name: list-nth
version: 1.10
description: Definitions and theorems about the list nth function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-nth-def-1.11
}

thm {
  import: def
  package: list-nth-thm-1.10
}

main {
  import: def
  import: thm
}
