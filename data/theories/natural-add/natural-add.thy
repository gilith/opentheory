name: natural-add
version: 1.62
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
  package: natural-add-def-1.24
  checksum: 1a6bdb74607779881d2a2c37d77ebfae4f037759
}

thm {
  import: def
  package: natural-add-thm-1.54
  checksum: abfe0764e27557e517fe5faa0ae4812f19fada5b
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.8
  checksum: c265e1b0fed515db7dbd616652669af684e5267c
}

main {
  import: def
  import: thm
  import: sub
}
