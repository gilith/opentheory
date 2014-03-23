name: hardware
version: 1.64
description: Hardware devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
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
  package: hardware-def-1.16
}

thm {
  import: def
  package: hardware-thm-1.25
}

wire {
  import: thm
  package: hardware-wire-1.12
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.38
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.11
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.12
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.10
}

main {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  import: counter
  import: multiplier
}
