name: hardware-counter
version: 1.16
description: Hardware counter devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: hardware-adder
requires: hardware-bus
requires: hardware-def
requires: hardware-thm
requires: hardware-wire
requires: list
requires: natural
requires: natural-bits
requires: pair
requires: set
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: hardware-counter-def-1.8
  checksum: ac241169a56a06d08444a9773a66593ec7286618
}

thm {
  import: def
  package: hardware-counter-thm-1.17
  checksum: 7bcc270ac0612f32730381710d6a95525ceb5aa6
}

main {
  import: def
  import: thm
}
