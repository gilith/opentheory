name: natural-distance
version: 1.50
description: Natural number distance
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.40
  checksum: 103e0ab3401e5fd027d1ca91064542e9abff97c7
}

thm {
  import: def
  package: natural-distance-thm-1.57
  checksum: 8a42addb7c7511b9ee503c0197a181bd5e3a6aaf
}

main {
  import: def
  import: thm
}
