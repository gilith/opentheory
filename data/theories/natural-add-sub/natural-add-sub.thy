name: natural-add-sub
version: 1.10
description: Natural number subtraction
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add-def
requires: natural-add-thm
requires: natural-def
requires: natural-dest
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-sub-def-1.11
  checksum: 31a5d467574e7d50e407274c6f6ad5f3902e97ea
}

thm {
  import: def
  package: natural-add-sub-thm-1.9
  checksum: b2996afedade167ad7e4779748d63df24153b522
}

main {
  import: def
  import: thm
}
