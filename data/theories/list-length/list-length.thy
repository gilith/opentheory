name: list-length
version: 1.25
description: The list length function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list-def
requires: list-thm
requires: list-dest
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-length-def-1.22
}

thm {
  import: def
  package: list-length-thm-1.27
}

main {
  import: def
  import: thm
}
