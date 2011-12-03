name: list-nub
version: 1.30
description: The list nub function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set
requires: list-def
requires: list-length
requires: list-reverse
requires: list-member
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nub-def-1.27
}

thm {
  import: def
  package: list-nub-thm-1.32
}

main {
  import: def
  import: thm
}
