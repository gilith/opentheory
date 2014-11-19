name: set-finite
version: 1.55
description: Finite sets
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set-def
requires: set-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-finite-def-1.36
  checksum: ab81ab3629b48e3fab01487bff018fb65c537ee9
}

thm {
  import: def
  package: set-finite-thm-1.61
  checksum: 747a96d0e1fe479833f659c679882985c5131757
}

main {
  import: def
  import: thm
}
