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
}

thm {
  import: def
  package: montgomery-hardware-thm-1.4
}

main {
  import: def
  import: thm
}
