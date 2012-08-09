name: natural
version: 1.71
description: The natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.5
}

def {
  import: axiom-infinity
  package: natural-def-1.22
}

thm {
  import: def
  package: natural-thm-1.15
}

dest {
  import: thm
  package: natural-dest-1.9
}

numeral {
  import: thm
  package: natural-numeral-1.14
}

order {
  import: def
  import: thm
  package: natural-order-1.38
}

add {
  import: def
  import: thm
  import: dest
  import: numeral
  import: order
  package: natural-add-1.51
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.48
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-div-1.37
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: div
  package: natural-exp-1.36
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.27
}

distance {
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-distance-1.43
}

funpow {
  import: def
  import: thm
  import: numeral
  import: add
  import: mult
  package: natural-funpow-1.5
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
