name: list-zip
version: 1.16
description: The list zip function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
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
  package: list-zip-def-1.12
}

thm {
  import: def
  package: list-zip-thm-1.16
}

main {
  import: def
  import: thm
}
