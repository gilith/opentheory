name: hardware-wire
version: 1.19
description: Hardware wire devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-thm
requires: natural
requires: stream
show: "Data.Bool"
show: "Data.Stream"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-wire-def-1.9
  checksum: 51c03e3d03bce3dc49a5ba8c0cab74f84422b4b5
}

thm {
  import: def
  package: hardware-wire-thm-1.23
  checksum: 7814e085be5482e3590d1d305457708bec6e3cfe
}

main {
  import: def
  import: thm
}
