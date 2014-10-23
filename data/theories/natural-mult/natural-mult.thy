name: natural-mult
version: 1.57
description: Natural number multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-mult-def-1.26
  checksum: ebb1edc51eb52cdf87b7b142e3b15b29500c5f6c
}

thm {
  import: def
  package: natural-mult-thm-1.50
  checksum: 28f4127875e65ae4e0579446cf5414deed5af77d
}

main {
  import: def
  import: thm
}
