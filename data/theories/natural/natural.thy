name: natural
version: 1.100
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
  package: natural-thm-1.21
  checksum: 7fe40861708e2c0e88f9ef8d31239946a31c7547
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
  package: natural-order-1.50
  checksum: bfdc3e3c0b5a5a44ba49f38d7cade908e7bd0fba
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.63
  checksum: 736e27b82c3af2c8c7624a77fa9f4e900a05abda
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.60
  checksum: ceea5c6888044c22c464a8eb2126506892cbb1b8
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.50
  checksum: 1beeb5553671eff75816959b7f1d2146f4fc28fd
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.51
  checksum: 7e34ceeac37372ab795af12d98a8c70999237e45
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
