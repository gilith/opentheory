name: natural
version: 1.39
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
  package: natural-def-1.13
}

thm {
  import: def
  package: natural-thm-1.7
}

dest {
  import: thm
  package: natural-dest-1.4
}

numeral {
  import: thm
  package: natural-numeral-1.9
}

order {
  import: def
  import: thm
  package: natural-order-1.23
}

add {
  import: def
  import: thm
  import: numeral
  import: order
  package: natural-add-1.31
}

mult {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  package: natural-mult-1.29
}

exp {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-exp-1.19
}

sub {
  import: def
  import: thm
  import: dest
  import: order
  import: add
  import: mult
  package: natural-sub-1.16
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
  package: natural-div-1.18
}

factorial {
  import: def
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.13
}

distance {
  import: thm
  import: numeral
  import: order
  import: add
  import: mult
  import: sub
  package: natural-distance-1.24
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
