name: natural
version: 1.97
description: The natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.10
  checksum: 3e38e591a4adc6637b654bedc8fc2b81b1f0dacc
}

def {
  import: axiom-infinity
  package: natural-def-1.27
  checksum: d3e2381787945539647c508788c023d9b8268f9f
}

thm {
  import: def
  package: natural-thm-1.19
  checksum: d28e45201e6cee3214118610e368cf0ab786d88f
}

dest {
  import: thm
  package: natural-dest-1.15
  checksum: 983c1f920adaa46ee710c7e14fb45d3467258565
}

numeral {
  import: thm
  package: natural-numeral-1.18
  checksum: 6bdd224125db2dbb3a4f42753c1699c918baa235
}

order {
  import: def
  import: thm
  package: natural-order-1.47
  checksum: ebfbf32b6db524140f48d40226fc760a75742030
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.60
  checksum: 5a6edb08336754d717ab1ef284d4092c1b91572b
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.57
  checksum: e8b1e4a9838a9915612aa46d1aae844fe0819615
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.47
  checksum: 1a10b1f6092f3bc5c906f017cebd4a4377c552e0
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.48
  checksum: 94a0e1924fee44053e5fe971d8d199d5f1af6d32
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.35
  checksum: e5107f4e1804247b2fc8feef719fa4aa5a892793
}

distance {
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-distance-1.50
  checksum: 2cb847a6ab3b9b56afe57f916857bf4c8a526dc2
}

funpow {
  import: def
  import: thm
  import: numeral
  import: add
  import: mult
  package: natural-funpow-1.14
  checksum: 778630a3d457d23e91e4db11b741daf1e316970a
}

main {
  import: axiom-infinity
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  import: exp
  import: factorial
  import: distance
  import: funpow
}
