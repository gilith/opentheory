name: natural-add
version: 1.65
description: Natural number addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-dest
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-def-1.25
  checksum: c980a19b5bd9ccb991e149f7328a57e3da4e0d6f
}

thm {
  import: def
  package: natural-add-thm-1.55
  checksum: 46a6460c94750b81d01162a3e1bf270403e773fa
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.10
  checksum: ee32b26fddd35de6625ef5d1dbdb02ef34677940
}

main {
  import: def
  import: thm
  import: sub
}
