name: hardware-multiplier
version: 1.18
description: Hardware multiplier devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware-adder
requires: hardware-bus
requires: hardware-counter
requires: hardware-thm
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-multiplier-def-1.12
}

thm {
  import: def
  package: hardware-multiplier-thm-1.15
}

main {
  import: def
  import: thm
}
