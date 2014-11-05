name: natural-funpow
version: 1.17
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
  package: natural-funpow-def-1.18
  checksum: 05f593fdad9ca4cdd16c454eca6a68e0507ea2dd
}

thm {
  import: def
  package: natural-funpow-thm-1.8
  checksum: 2ae1fc896080744b07b2237ac9af3c7786aa4b77
}

main {
  import: def
  import: thm
}
