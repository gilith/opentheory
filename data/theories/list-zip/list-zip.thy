name: list-zip
version: 1.30
description: The list zip functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-fold
requires: list-length
requires: list-nth
requires: list-thm
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: list-zip-def-1.23
}

thm {
  import: def
  package: list-zip-thm-1.26
}

main {
  import: def
  import: thm
}
