name: montgomery
version: 1.19
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
  package: montgomery-def-1.8
  checksum: b0549bade31e35d745e85bfcc571516ff6fd521b
}

thm {
  import: def
  package: montgomery-thm-1.18
  checksum: ca34da8ad749f9cfa1ad61385276cf8032505e95
}

hardware {
  import: thm
  package: montgomery-hardware-1.1
  checksum: 8b6c0ad0097712187b2432c219790307bae47b5f
}

main {
  import: def
  import: thm
  import: hardware
}
