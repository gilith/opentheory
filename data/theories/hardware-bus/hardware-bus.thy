name: hardware-bus
version: 1.40
description: Hardware bus devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-thm
requires: hardware-wire
requires: list
requires: natural
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-bus-def-1.23
}

thm {
  import: def
  package: hardware-bus-thm-1.39
}

main {
  import: def
  import: thm
}