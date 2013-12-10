name: hardware
version: 1.39
description: Hardware devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: set
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Stream"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-def-1.15
}

thm {
  import: def
  package: hardware-thm-1.21
}

wire {
  import: thm
  package: hardware-wire-1.6
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.31
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.5
}

counter {
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.1
}

main {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  import: counter
}
