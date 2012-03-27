name: list-length
version: 1.33
description: The list length function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-length-def-1.28
}

thm {
  import: def
  package: list-length-thm-1.36
}

main {
  import: def
  import: thm
}
