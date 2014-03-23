name: hardware-montgomery
version: 1.3
description: Hardware Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware
requires: montgomery
requires: natural
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-montgomery-def-1.5
}

thm {
  import: def
  package: hardware-montgomery-thm-1.5
}

main {
  import: def
  import: thm
}
