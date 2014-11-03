name: hardware-wire
version: 1.21
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
  package: hardware-wire-def-1.10
  checksum: d488ae5b76416941067d5675e1c57ccb0a7adf9a
}

thm {
  import: def
  package: hardware-wire-thm-1.25
  checksum: 068f01491161e70689a5ff69673f4bf45bce6046
}

main {
  import: def
  import: thm
}
