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
}

thm {
  import: def
  package: hardware-wire-thm-1.25
}

main {
  import: def
  import: thm
}
