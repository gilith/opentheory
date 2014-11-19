name: hardware-counter
version: 1.19
description: Hardware counter devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: hardware-adder
requires: hardware-bus
requires: hardware-def
requires: hardware-thm
requires: hardware-wire
requires: list
requires: natural
requires: natural-bits
requires: pair
requires: set
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: hardware-counter-def-1.10
  checksum: 43bac8c8172631cb30bf4027e6aba7cf7d6637e8
}

thm {
  import: def
  package: hardware-counter-thm-1.19
  checksum: 4f9c22a5bfc5976f2ebf31e7294eb94964378bac
}

main {
  import: def
  import: thm
}
