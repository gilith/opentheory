name: natural
version: 1.15
description: The natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"
show: "Number.Natural"

axiom-infinity {
  package: axiom-infinity-1.2
}

def {
  import: axiom-infinity
  package: natural-def-1.4
}

recursion {
  import: def
  package: natural-recursion-1.2
}

cases {
  import: def
  import: recursion
  package: natural-cases-1.3
}

numeral {
  import: cases
  package: natural-numeral-1.4
}

pre {
  import: cases
  package: natural-pre-1.4
}

order {
  import: def
  import: cases
  import: numeral
  package: natural-order-1.8
}

add {
  import: def
  import: cases
  import: numeral
  import: order
  package: natural-add-1.11
}

mult {
  import: def
  import: cases
  import: order
  import: add
  package: natural-mult-1.10
}

exp {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-exp-1.5
}

even-odd {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  package: natural-even-odd-1.4
}

sub {
  import: def
  import: cases
  import: pre
  import: order
  import: add
  import: mult
  import: even-odd
  package: natural-sub-1.5
}

factorial {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.5
}

div-mod {
  import: def
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  import: even-odd
  package: natural-div-mod-1.5
}

min-max {
  import: order
  package: natural-min-max-1.7
}

distance {
  import: def
  import: order
  import: add
  import: mult
  import: sub
  package: natural-distance-1.5
}

main {
  import: axiom-infinity
  import: def
  import: recursion
  import: cases
  import: numeral
  import: pre
  import: order
  import: add
  import: mult
  import: exp
  import: even-odd
  import: sub
  import: factorial
  import: div-mod
  import: min-max
  import: distance
}
