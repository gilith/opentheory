name: hardware
version: 1.84
description: Hardware devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
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
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art

def {
  package: hardware-def-1.21
}

thm {
  import: def
  package: hardware-thm-1.33
}

wire {
  import: thm
  package: hardware-wire-1.23
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.46
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.18
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.21
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.18
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
