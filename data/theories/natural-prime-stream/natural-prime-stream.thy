name: natural-prime-stream
version: 1.24
description: The ordered stream of all prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
requires: natural-prime-thm
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Stream"
show: "Number.Natural"

def {
  package: natural-prime-stream-def-1.19
  checksum: 2c37d248d8af4af15027d01b4d9aa74be3a2960b
}

thm {
  import: def
  package: natural-prime-stream-thm-1.23
  checksum: 95d73d834232d329d6923b202a1b0b879117a331
}

main {
  import: def
  import: thm
}
