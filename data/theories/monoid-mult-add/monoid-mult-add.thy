name: monoid-mult-add
version: 1.9
description: Monoid multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: monoid-mult-def
requires: monoid-mult-thm
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: monoid-mult-add-def-1.5
  checksum: 23c9fc23861cd8f4c95571bb0139a838819f7f5b
}

thm {
  import: def
  package: monoid-mult-add-thm-1.7
  checksum: 892b6ecec7dcf30dd62bf81caebf95e84ee6db04
}

main {
  import: def
  import: thm
}
