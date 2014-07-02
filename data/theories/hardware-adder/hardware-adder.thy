name: hardware-adder
version: 1.12
description: Hardware adder devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-bus
requires: hardware-thm
requires: hardware-wire
requires: natural
requires: natural-bits
show: "Data.Bool"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-adder-def-1.10
}

thm {
  import: def
  package: hardware-adder-thm-1.15
}

main {
  import: def
  import: thm
}
