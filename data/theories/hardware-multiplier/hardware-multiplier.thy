name: hardware-multiplier
version: 1.15
description: Hardware multiplier devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-adder
requires: hardware-bus
requires: hardware-counter
requires: hardware-thm
requires: natural
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-multiplier-def-1.11
  checksum: 49a5687ae2bff4f6b9217c7d2cf1794656857b8c
}

thm {
  import: def
  package: hardware-multiplier-thm-1.13
  checksum: a720257e410ad2b5b8f4a99677ec49fd446e7c08
}

main {
  import: def
  import: thm
}
