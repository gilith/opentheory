name: list-nth
version: 1.18
description: Definitions and theorems about the list nth function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-nth-def-1.16
}

thm {
  import: def
  package: list-nth-thm-1.19
}

main {
  import: def
  import: thm
}
