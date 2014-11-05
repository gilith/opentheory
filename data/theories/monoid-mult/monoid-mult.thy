name: monoid-mult
version: 1.11
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
  package: monoid-mult-def-1.9
  checksum: f458d6973fdf3216542ffec5db0914741a79cf5d
}

thm {
  import: def
  package: monoid-mult-thm-1.6
  checksum: 50ff0c6f4cf547e9dd702ded7b8386085642a425
}

add {
  import: def
  import: thm
  package: monoid-mult-add-1.12
  checksum: 5ca75be5777937ab3cbee3f69bdbed84aff98d54
}

main {
  import: def
  import: thm
  import: add
}
