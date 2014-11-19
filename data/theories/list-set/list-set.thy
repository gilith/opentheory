name: list-set
version: 1.55
description: List to set conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-set-def-1.53
  checksum: eb885d37d937fc38d799e7360fc407504130d7da
}

thm {
  import: def
  package: list-set-thm-1.51
  checksum: dfce74d73076ed1b5b4ad160cb1b1ef3873c5861
}

main {
  import: def
  import: thm
}
