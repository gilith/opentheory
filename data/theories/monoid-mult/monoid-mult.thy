name: monoid-mult
version: 1.10
description: Monoid multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: monoid-thm
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-def-1.8
  checksum: 2c4a85035ecb4601745968e54231bcb921f532f8
}

thm {
  import: def
  package: monoid-mult-thm-1.6
  checksum: 50ff0c6f4cf547e9dd702ded7b8386085642a425
}

add {
  import: def
  import: thm
  package: monoid-mult-add-1.11
  checksum: 1ddfbae2ab2fc77c5619b1ba527e7e5cc3485a60
}

main {
  import: def
  import: thm
  import: add
}
