name: montgomery-hardware
version: 1.3
description: Hardware Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: hardware
requires: montgomery-thm
requires: natural
requires: natural-bits
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: montgomery-hardware-def-1.3
  checksum: 65d833d288692d8f32f4cebc21e0f1250f213845
}

thm {
  import: def
  package: montgomery-hardware-thm-1.4
  checksum: f7c67eb9babb672060af10a5dc1da7d13e3c64e8
}

main {
  import: def
  import: thm
}
