name: hardware-counter
version: 1.21
description: Hardware counter devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware-adder
requires: hardware-bus
requires: hardware-def
requires: hardware-thm
requires: hardware-wire
requires: natural-bits
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
  package: hardware-counter-def-1.11
}

thm {
  import: def
  package: hardware-counter-thm-1.21
}

main {
  import: def
  import: thm
}
