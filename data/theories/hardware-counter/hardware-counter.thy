name: hardware-counter
version: 1.18
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
  package: hardware-counter-def-1.9
  checksum: 1759fc1fbc6d05b8ba6ecd4b6cd8aa4689d773da
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
