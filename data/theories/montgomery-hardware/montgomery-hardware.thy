name: montgomery-hardware
version: 1.1
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
  package: montgomery-hardware-def-1.2
  checksum: 93a3cd3448017913feb8e28e6f08b3c1cc3f796f
}

thm {
  import: def
  package: montgomery-hardware-thm-1.2
  checksum: d0e1f4e619b5ceb6951b6eb754535e5d250bcb2b
}

main {
  import: def
  import: thm
}
