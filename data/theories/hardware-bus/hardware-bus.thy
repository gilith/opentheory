name: hardware-bus
version: 1.41
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
  package: hardware-bus-def-1.24
  checksum: 9381e2daa9c5882799d46d69c6888420fa87889c
}

thm {
  import: def
  package: hardware-bus-thm-1.40
  checksum: e6781a8b82e9a8f5f6bca2d540933ba81b7f0c95
}

main {
  import: def
  import: thm
}
