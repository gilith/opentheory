name: list-zipwith
version: 1.23
description: The list zipWith function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list-def
requires: list-dest
requires: list-length
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-zipwith-def-1.24
}

thm {
  import: def
  package: list-zipwith-thm-1.23
}

main {
  import: def
  import: thm
}
