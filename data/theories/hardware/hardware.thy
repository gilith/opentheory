name: hardware
version: 1.21
description: Hardware devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-bits
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Stream"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-def-1.11
}

thm {
  import: def
  package: hardware-thm-1.15
}

wire {
  import: thm
  package: hardware-wire-1.0
}

bus {
  import: def
  import: thm
  import: wire
  package: hardware-bus-1.21
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.0
}

main {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
}
