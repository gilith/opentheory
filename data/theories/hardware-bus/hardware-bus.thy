name: hardware-bus
version: 1.46
description: Hardware bus devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware-thm
requires: hardware-wire
requires: natural-bits
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-bus-def-1.26
}

thm {
  import: def
  package: hardware-bus-thm-1.45
}

main {
  import: def
  import: thm
}
