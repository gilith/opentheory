name: natural
version: 1.102
description: The natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.12
  checksum: 53e364be097eb2d2284001aa97935dcee31a969c
}

def {
  import: axiom-infinity
  package: natural-def-1.29
  checksum: 881d4b54385e84df7360d40052b061eb8bc67893
}

thm {
  import: def
  package: natural-thm-1.22
  checksum: 5c21d070b2f254a88335be15c935de8808de25b6
}

dest {
  import: thm
  package: natural-dest-1.18
  checksum: b2e09315a88f198b20aa98e636c5828d3aeec233
}

numeral {
  import: thm
  package: natural-numeral-1.20
  checksum: d60228fe9c6d0bfacfeb3e1ce8eb00022e67b883
}

order {
  import: def
  import: thm
  package: natural-order-1.51
  checksum: b7a83b40e6b31c86ffcda7d342507136de8b7365
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.65
  checksum: 0cdb8b389a4da2c26c7d544006acc579454f0e86
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.61
  checksum: 9c28888dd90df611c9be94870701eb5d4713d069
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.51
  checksum: 4f89e0853a2442bbe65cdea73e708ce31692889a
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.52
  checksum: 5f50d2adce79514bea5aa57911259a2067f23c15
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.38
  checksum: a58d1fa7b1e82217c934dbc0e86c06b570513c3d
}

distance {
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-distance-1.52
  checksum: ead8b0b469ebabd24b83f8f6356f412e8710f607
}

funpow {
  import: def
  import: thm
  import: numeral
  import: add
  import: mult
  package: natural-funpow-1.17
  checksum: 06b3d82f505ad5c709d22cb2693e99cb9e5704a9
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
