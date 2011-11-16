name: natural-gcd
version: 1.5
description: Natural number greatest common divisor
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-def-1.2
}

thm {
  import: def
  package: natural-gcd-thm-1.5
}

main {
  import: def
  import: thm
}
