name: hardware-bus
version: 1.21
description: Hardware bus devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-def
requires: hardware-thm
requires: hardware-wire
requires: list
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-bus-def-1.15
}

thm {
  import: def
  package: hardware-bus-thm-1.20
}

main {
  import: def
  import: thm
}
