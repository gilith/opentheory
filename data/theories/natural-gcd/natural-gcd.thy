name: natural-gcd
version: 1.50
description: Natural number greatest common divisor
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-def-1.40
  checksum: 5dfa439d94de76b8c69f68297d710a037b1e129a
}

thm {
  import: def
  package: natural-gcd-thm-1.48
  checksum: 8230afacd749f015365abd427a19b9c9f4ca490b
}

lcm {
  import: def
  import: thm
  package: natural-gcd-lcm-1.38
  checksum: 2ba23a73c971509db8480a85451178d68cd8051e
}

main {
  import: def
  import: thm
  import: lcm
}
