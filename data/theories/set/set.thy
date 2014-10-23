name: set
version: 1.66
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
  package: set-def-1.49
  checksum: deacc7ee162788c1bbf3db90455adabe7e6ac7d9
}

thm {
  import: def
  package: set-thm-1.62
  checksum: e26809a2d80b1c8e6b6b45ac1f18f97977a40957
}

finite {
  import: def
  import: thm
  package: set-finite-1.52
  checksum: f7591a684bc307947834918486b1adca2a431efd
}

fold {
  import: thm
  import: finite
  package: set-fold-1.43
  checksum: da46384cb6a58593674f9abe782bdf10635ff41c
}

size {
  import: def
  import: thm
  import: finite
  import: fold
  package: set-size-1.54
  checksum: 3cdd22bd87dc93caf7753d07a1538d83d2b84a01
}

main {
  import: def
  import: thm
  import: finite
  import: fold
  import: size
}
