name: hardware-adder
version: 1.13
description: Hardware adder devices
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware-bus
requires: hardware-thm
requires: hardware-wire
requires: natural
requires: natural-bits
show: "Data.Bool"
show: "Hardware"
show: "Number.Natural"

def {
  package: hardware-adder-def-1.11
  checksum: 3a31441cd6078340912d2757e624ace426b9deab
}

thm {
  import: def
  package: hardware-adder-thm-1.16
  checksum: c0f857854916679491bdce2cc9be80514aea19b9
}

main {
  import: def
  import: thm
}
