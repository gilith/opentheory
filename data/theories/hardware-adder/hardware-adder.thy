name: hardware-adder
version: 1.15
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
  package: hardware-adder-def-1.12
  checksum: ad64c40400339d50b57ea1b3e66d9974ed74192a
}

thm {
  import: def
  package: hardware-adder-thm-1.18
  checksum: 13f877448595c9ffe62091926ae96deb9b4c5571
}

main {
  import: def
  import: thm
}
