name: hardware
version: 1.45
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
  package: hardware-thm-1.22
}

wire {
  import: thm
  package: hardware-wire-1.8
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.33
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.7
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.4
}

multiplier {
  import: thm
  import: bus
  import: adder
  package: hardware-multiplier-1.1
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
