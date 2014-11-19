name: hardware-wire
version: 1.22
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
  package: hardware-wire-def-1.11
  checksum: e9eff1c5ea6b86c9ba7c4ba7b4c0846c4ee67c99
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
