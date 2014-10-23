name: natural-add
version: 1.60
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
  package: natural-add-def-1.22
  checksum: 213d05a88caccdb388ff9d7e54e56cafc783ec9d
}

thm {
  import: def
  package: natural-add-thm-1.52
  checksum: b87e2e8abe22f9345a4e9f2bd9ed5ecdba2a6cab
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.6
  checksum: ad1315154eca5e5dd7c9b885e6b30886da8161f7
}

main {
  import: def
  import: thm
  import: sub
}
