name: natural
version: 1.13
description: The natural numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"

axiom-infinity {
  package: axiom-infinity-1.1
}

def {
  import: axiom-infinity
  package: natural-def-1.3
}

recursion {
  import: def
  package: natural-recursion-1.1
}

cases {
  import: def
  import: recursion
  package: natural-cases-1.2
}

numeral {
  import: cases
  package: natural-numeral-1.3
}

pre {
  import: cases
  package: natural-pre-1.3
}

order {
  import: def
  import: cases
  import: numeral
  package: natural-order-1.7
}

add {
  import: def
  import: cases
  import: numeral
  import: order
  package: natural-add-1.10
}

mult {
  import: def
  import: cases
  import: order
  import: add
  package: natural-mult-1.9
}

exp {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-exp-1.4
}

even-odd {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  package: natural-even-odd-1.3
}

sub {
  import: def
  import: cases
  import: pre
  import: order
  import: add
  import: mult
  import: even-odd
  package: natural-sub-1.4
}

factorial {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.4
}

div-mod {
  import: def
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  import: even-odd
  package: natural-div-mod-1.4
}

min-max {
  import: order
  package: natural-min-max-1.6
}

distance {
  import: def
  import: order
  import: add
  import: mult
  import: sub
  package: natural-distance-1.4
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
