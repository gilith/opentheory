name: montgomery-hardware
version: 1.4
description: Hardware Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware
requires: montgomery-thm
requires: natural
requires: natural-bits
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: montgomery-hardware-def-1.4
  checksum: 8977f3a6f29aeb9828a57ec5590658c7828d2c07
}

thm {
  import: def
  package: montgomery-hardware-thm-1.4
  checksum: f7c67eb9babb672060af10a5dc1da7d13e3c64e8
}

main {
  import: def
  import: thm
}
