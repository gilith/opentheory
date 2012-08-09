name: natural-divides
version: 1.34
description: The divides relation on natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-def-1.30
}

thm {
  import: def
  package: natural-divides-thm-1.38
}

main {
  import: def
  import: thm
}
