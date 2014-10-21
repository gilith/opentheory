name: set
version: 1.64
description: Set types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: set-def-1.47
  checksum: 9ca8385e3cae829767c09e97962a08ff6474fbaf
}

thm {
  import: def
  package: set-thm-1.61
  checksum: 06ffd9ac483e46b181bb3251b1bf2ecf242f485f
}

finite {
  import: def
  import: thm
  package: set-finite-1.51
  checksum: 319bffb0c102a8bacf86370b031c7171a1a3af06
}

fold {
  import: thm
  import: finite
  package: set-fold-1.42
  checksum: 0fb9780a00619b59740bc1134054f182e60f3dd6
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.53
  checksum: 9dcd589bc8d4596f8cebdfd2047f0a65abb5ff61
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}
