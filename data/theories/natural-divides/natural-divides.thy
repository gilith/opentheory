name: natural-divides
version: 1.24
description: The divides relation on natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-def-1.20
}

thm {
  import: def
  package: natural-divides-thm-1.27
}

main {
  import: def
  import: thm
}
