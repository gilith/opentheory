name: hardware-wire
version: 1.8
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
  package: hardware-wire-def-1.6
}

thm {
  import: def
  package: hardware-wire-thm-1.12
}

main {
  import: def
  import: thm
}
