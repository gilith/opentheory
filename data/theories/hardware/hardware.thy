name: hardware
version: 1.75
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
  package: hardware-def-1.20
  checksum: c5e669c2918798e17458389f6035ffcaa0ff4342
}

thm {
  import: def
  package: hardware-thm-1.30
  checksum: f7df20db276cdf89bc70160c34bd8f17cb8bdd71
}

wire {
  import: thm
  package: hardware-wire-1.21
  checksum: dd4ef4594477fd865efead5135e3252576a3ac0e
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.43
  checksum: a2d6dc11290bc13e7c7bcb2c5a80dcdf0ffce512
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.15
  checksum: c7d4fdc1bf50d04661ac25e3def050c5ee5a67f1
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.18
  checksum: 99aa849545ff47bb45d5371e0f81e176bb9b1b34
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.14
  checksum: 02a7057c803595de4b395765f7f0ec4110f38ba5
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
