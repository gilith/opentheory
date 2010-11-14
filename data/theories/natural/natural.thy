name: natural
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Function"
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
  import: def
  import: recursion
  import: cases
  import: add
  package: natural-numeral-1.0
}

pre {
  import: def
  import: recursion
  import: cases
  import: numeral
  package: natural-pre-1.0
}

add {
  import: def
  import: recursion
  import: cases
  import: numeral
  package: natural-add-1.0
}

main {
  import: def
  import: recursion
  import: cases
  import: numeral
  import: pre
  import: add
}
