name: natural-divides
version: 1.23
description: The divides relation on natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-def-1.19
}

thm {
  import: def
  package: natural-divides-thm-1.26
}

main {
  import: def
  import: thm
}
