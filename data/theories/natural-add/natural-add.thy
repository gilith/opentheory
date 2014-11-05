name: natural-add
version: 1.63
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
  package: natural-add-thm-1.54
  checksum: abfe0764e27557e517fe5faa0ae4812f19fada5b
}

sub {
  import: def
  import: thm
  package: natural-add-sub-1.9
  checksum: 919cb2f053e7d58f34a7eef8682a7678ddbff86e
}

main {
  import: def
  import: thm
  import: sub
}
