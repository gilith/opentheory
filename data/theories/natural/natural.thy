name: natural
version: 1.94
description: The natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.8
}

def {
  import: axiom-infinity
  package: natural-def-1.25
}

thm {
  import: def
  package: natural-thm-1.17
}

dest {
  import: thm
  package: natural-dest-1.13
}

numeral {
  import: thm
  package: natural-numeral-1.18
}

order {
  import: def
  import: thm
  package: natural-order-1.44
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.57
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.54
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.45
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.45
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.32
}

distance {
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-distance-1.48
}

funpow {
  import: def
  import: thm
  import: numeral
  import: add
  import: mult
  package: natural-funpow-1.11
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
