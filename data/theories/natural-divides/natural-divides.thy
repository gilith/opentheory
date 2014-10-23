name: natural-divides
version: 1.45
description: The divides relation on natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-divides-def-1.39
  checksum: ab3027919233e8367284d484879ad17b627e0d6c
}

thm {
  import: def
  package: natural-divides-thm-1.48
  checksum: 6bf780419ec71cfbe29e611e4598c7f15f85b6c4
}

main {
  import: def
  import: thm
}
