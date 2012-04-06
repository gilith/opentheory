name: list-zip
version: 1.3
description: The list zip function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: list-zip-def-1.2
}

thm {
  import: def
  package: list-zip-thm-1.3
}

main {
  import: def
  import: thm
}
