name: natural-funpow
version: 1.14
description: Function power
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-thm
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

def {
  package: natural-funpow-def-1.15
  checksum: 3c676f7b921ce36fcfa436592aacefe97b843e4c
}

thm {
  import: def
  package: natural-funpow-thm-1.6
  checksum: 01348794340bc5e713442741e067b34319b3a9ed
}

main {
  import: def
  import: thm
}
