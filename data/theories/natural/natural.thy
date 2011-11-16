name: natural
version: 1.34
description: The natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.4
}

def {
  import: axiom-infinity
  package: natural-def-1.10
}

thm {
  import: def
  package: natural-thm-1.5
}

dest {
  import: thm
  package: natural-dest-1.2
}

numeral {
  import: thm
  package: natural-numeral-1.7
}

order {
  import: def
  import: thm
  package: natural-order-1.20
}

add {
  import: def
  import: thm
  import: numeral
  import: order
  package: natural-add-1.25
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.24
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-exp-1.16
}

sub {
  import: def
  import: thm
  import: dest
  import: order
  import: add
  import: mult
  package: natural-sub-1.14
}

div {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  import: sub
  package: natural-div-1.15
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.11
}

distance {
  import: def
  import: order
  import: add
  import: mult
  import: sub
  package: natural-distance-1.18
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
  import: exp
  import: sub
  import: div
  import: factorial
  import: distance
}
