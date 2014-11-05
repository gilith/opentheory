name: set
version: 1.69
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
  package: set-def-1.51
  checksum: 0ec28d6a978c3306f3c7f413b53bcbae3d938a16
}

thm {
  import: def
  package: set-thm-1.64
  checksum: 0db264f383c877959378fc119e436b498a16d928
}

finite {
  import: def
  import: thm
  package: set-finite-1.54
  checksum: 6d5c255fcc24cef363627553425f98088e7dae5d
}

fold {
  import: thm
  import: finite
  package: set-fold-1.46
  checksum: 24c4f848d83eec4cebfaa91096efe5bdb3015b6e
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.56
  checksum: 73576175382ef46aef1cb2d4cfb68088a65005b5
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}
