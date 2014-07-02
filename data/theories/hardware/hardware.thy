name: hardware
version: 1.71
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
  package: hardware-thm-1.27
}

wire {
  import: thm
  package: hardware-wire-1.18
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.40
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.12
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.15
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.11
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
