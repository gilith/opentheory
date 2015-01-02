name: set-finite
version: 1.56
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
  package: set-finite-def-1.37
  checksum: 5ea93dfad7fc9a0d9031fe5f50c09ab0778e31c4
}

thm {
  import: def
  package: set-finite-thm-1.62
  checksum: f9b9b21b288e231cfebcf5ef56bc3f0428e65245
}

main {
  import: def
  import: thm
}
