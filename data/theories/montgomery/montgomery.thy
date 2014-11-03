name: montgomery
version: 1.21
description: Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-11-09
requires: bool
requires: hardware
requires: natural
requires: natural-bits
requires: natural-divides
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"
show: "Set"

def {
  package: montgomery-def-1.9
  checksum: c76cc84e374df2d4933fd0db573bfb187c0713dc
}

thm {
  import: def
  package: montgomery-thm-1.20
  checksum: 345a31c5b821a265ec6a51fe132452846fd58d16
}

hardware {
  import: thm
  package: montgomery-hardware-1.3
  checksum: 28dde5c6138584a371f03b5cce8b5f02d7abf353
}

main {
  import: def
  import: thm
  import: hardware
}
