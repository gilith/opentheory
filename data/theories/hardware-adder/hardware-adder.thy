name: hardware-adder
version: 1.18
description: Hardware adder devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware-bus
requires: hardware-thm
requires: hardware-wire
requires: natural-bits
show: "Data.Bool"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-adder-def-1.14
}

thm {
  import: def
  package: hardware-adder-thm-1.21
}

main {
  import: def
  import: thm
}
