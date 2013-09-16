name: hardware
version: 1.28
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
  package: hardware-def-1.12
}

thm {
  import: def
  package: hardware-thm-1.17
}

wire {
  import: thm
  package: hardware-wire-1.2
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.27
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.4
}

main {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
}
