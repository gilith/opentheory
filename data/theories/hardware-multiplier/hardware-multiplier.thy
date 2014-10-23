name: hardware-multiplier
version: 1.12
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
  package: hardware-multiplier-def-1.9
  checksum: 58311a307780c20efcb8d9bafa16f85aa687db16
}

thm {
  import: def
  package: hardware-multiplier-thm-1.11
  checksum: ff938b5fd396aeaef1d4acf350bbcf176099e671
}

main {
  import: def
  import: thm
}
