name: list-zip
version: 1.20
description: The list zip function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: list-zip-def-1.14
}

thm {
  import: def
  package: list-zip-thm-1.20
}

main {
  import: def
  import: thm
}
