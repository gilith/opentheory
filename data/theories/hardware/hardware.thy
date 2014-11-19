name: hardware
version: 1.76
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
  package: hardware-thm-1.31
  checksum: f1c3cab1316f4b9f2cbf6f0cf0fef15df51ada65
}

wire {
  import: thm
  package: hardware-wire-1.22
  checksum: fb92094e3fd3c0f7a56ed1270787584b4eb284b9
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.44
  checksum: 4b6f52f33706482a9d4e5c1ad42762b56120cfd6
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.16
  checksum: 6a1c673de2ad025b3edb25e324bf83bde1c8db65
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.19
  checksum: 7ed4b42c3d0ac538f3ae8292703a235ca3fde75f
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.15
  checksum: a768e1cf79c2d64a485d7db5aaefe0a3a3df4e6c
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
