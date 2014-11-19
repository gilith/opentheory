name: hardware-bus
version: 1.44
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
  package: hardware-bus-def-1.25
  checksum: d4921f168ad7d633de4d5e08f48adf7f0232fc1a
}

thm {
  import: def
  package: hardware-bus-thm-1.43
  checksum: e6c3c594a06f8f3bd4255757d233e0340ac2b0dc
}

main {
  import: def
  import: thm
}
