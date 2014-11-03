name: natural
version: 1.99
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
  package: natural-dest-1.17
  checksum: 68253235212a4d211fb2bbb0ddcf4d6cece86e00
}

numeral {
  import: thm
  package: natural-numeral-1.20
  checksum: d60228fe9c6d0bfacfeb3e1ce8eb00022e67b883
}

order {
  import: def
  import: thm
  package: natural-order-1.49
  checksum: e79662bd41ff1d43761c6f7819d5b0af0208d081
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.62
  checksum: bd223f65bcddfbfc995ceed7e07f6ae63f505438
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.59
  checksum: dead9de8fb52831cc851563d537a14bfa92f2108
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.49
  checksum: 5bc16c27229d3cb6268de0c574952e1c93896240
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.50
  checksum: e24892c69d5ad1240eecb057c9e100d31f384859
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.37
  checksum: 26303fc8c605c9adb6448d2b67224da01b1a6760
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
  package: natural-funpow-1.16
  checksum: e693a60a55e6ed9abbf44145116acc7b60957946
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
