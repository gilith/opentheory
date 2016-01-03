name: montgomery-hardware
version: 1.8
description: Hardware Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware
requires: montgomery-thm
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: montgomery-hardware-def-1.5
}

thm {
  import: def
  package: montgomery-hardware-thm-1.8
}

main {
  import: def
  import: thm
}
