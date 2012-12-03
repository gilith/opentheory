name: natural-divides
version: 1.43
description: The divides relation on natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-def-1.38
}

thm {
  import: def
  package: natural-divides-thm-1.46
}

main {
  import: def
  import: thm
}
