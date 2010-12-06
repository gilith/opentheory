name: natural
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"

axiom-infinity {
  package: axiom-infinity-1.0
}

def {
  import: axiom-infinity
  package: natural-def-1.0
}

recursion {
  import: def
  package: natural-recursion-1.0
}

cases {
  import: def
  import: recursion
  package: natural-cases-1.0
}

numeral {
  import: cases
  package: natural-numeral-1.0
}

pre {
  import: cases
  package: natural-pre-1.0
}

order {
  import: def
  import: cases
  import: numeral
  package: natural-order-1.0
}

add {
  import: def
  import: cases
  import: numeral
  import: order
  package: natural-add-1.0
}

mult {
  import: def
  import: cases
  import: order
  import: add
  package: natural-mult-1.0
}

exp {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-exp-1.0
}

set {
  import: def
  import: order
  package: natural-set-1.0
}

even-odd {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  import: set
  package: natural-even-odd-1.0
}

sub {
  import: def
  import: cases
  import: numeral
  import: pre
  import: order
  import: add
  import: mult
  import: even-odd
  package: natural-sub-1.0
}

factorial {
  import: def
  import: cases
  import: numeral
  import: order
  import: add
  import: mult
  package: natural-factorial-1.0
}

div-mod {
  import: def
  import: numeral
  import: order
  import: add
  import: mult
  import: exp
  import: set
  import: even-odd
  package: natural-div-mod-1.0
}

min-max {
  import: set
  package: natural-min-max-1.0
}

main {
  import: def
  import: recursion
  import: cases
  import: numeral
  import: pre
  import: order
  import: add
  import: mult
  import: exp
  import: set
  import: even-odd
  import: sub
  import: factorial
  import: div-mod
  import: min-max
}
