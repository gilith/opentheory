name: list-nub
version: 1.45
description: The list nub function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-length
requires: list-reverse
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nub-def-1.44
}

thm {
  import: def
  package: list-nub-thm-1.49
}

main {
  import: def
  import: thm
}
