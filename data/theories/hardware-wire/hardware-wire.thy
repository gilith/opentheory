name: hardware-wire
version: 1.2
description: Hardware wire devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-thm
show: "Data.Bool"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-wire-def-1.5
}

thm {
  import: def
  package: hardware-wire-thm-1.5
}

main {
  import: def
  import: thm
}
