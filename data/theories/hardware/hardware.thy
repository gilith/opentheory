name: hardware
version: 1.73
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
  package: hardware-def-1.18
  checksum: 4c483f0e92f727da92979fea4f5e36294ed262f2
}

thm {
  import: def
  package: hardware-thm-1.28
  checksum: 99a9b168465aebc1e22483f560612c3b0e059cb6
}

wire {
  import: thm
  package: hardware-wire-1.19
  checksum: 91156139cb1e202ca440a59f9bff0881a3383b2d
}

bus {
  import: thm
  import: wire
  package: hardware-bus-1.41
  checksum: 09d4a760238863ae8732a4314424149bc9ae61d0
}

adder {
  import: thm
  import: wire
  import: bus
  package: hardware-adder-1.13
  checksum: 0e47cf6a532757d2a9b42d91b3f9ca37504f372a
}

counter {
  import: def
  import: thm
  import: wire
  import: bus
  import: adder
  package: hardware-counter-1.16
  checksum: 37834563c7636ad0218368ffcf1ed17602aaaa05
}

multiplier {
  import: thm
  import: bus
  import: counter
  import: adder
  package: hardware-multiplier-1.12
  checksum: bb14b6a33fad407131b23f53253108372f91535d
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
