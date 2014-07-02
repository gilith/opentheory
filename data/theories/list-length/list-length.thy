name: list-length
version: 1.46
description: The list length function
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: list-length-def-1.42
}

thm {
  import: def
  package: list-length-thm-1.38
}

main {
  import: def
  import: thm
}
