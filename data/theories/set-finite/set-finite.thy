name: set-finite
version: 1.54
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
  package: set-finite-thm-1.60
  checksum: 3ad1fdcca80061971223c84d872fb5c9afe37330
}

main {
  import: def
  import: thm
}
