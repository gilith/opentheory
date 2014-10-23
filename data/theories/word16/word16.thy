name: word16
version: 1.109
description: 16-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
requires: list
requires: natural
requires: natural-bits
requires: natural-divides
requires: pair
requires: probability
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  package: word16-def-1.76
  checksum: f0d8a60653e65be409a74d52d6c6945490aad812
}

bits {
  import: def
  package: word16-bits-1.77
  checksum: fd841ec1c54e5a66c341133e07926098571a81fb
}

bytes {
  import: def
  import: bits
  package: word16-bytes-1.82
  checksum: 61390ed32d40d1040759b81e208cf0c4eb742b5a
}

main {
  import: def
  import: bits
  import: bytes
}
