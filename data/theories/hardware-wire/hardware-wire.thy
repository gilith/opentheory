name: hardware-wire
version: 1.23
description: Hardware wire devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: hardware-thm
requires: stream
show: "Data.Bool"
show: "Data.Stream"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-wire-def-1.12
}

thm {
  import: def
  package: hardware-wire-thm-1.26
}

main {
  import: def
  import: thm
}
