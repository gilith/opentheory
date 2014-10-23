name: word12
version: 1.99
description: 12-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word12"
show: "Data.Word12.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word12-def-1.76
  checksum: 8a78d790c039fdc3dbb9db1d3d0e74826c82ea50
}

bits {
  import: def
  package: word12-bits-1.74
  checksum: 359a3920cf181f330768646cec5a4d777cfdfc4a
}

main {
  import: def
  import: bits
}
