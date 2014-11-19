name: montgomery
version: 1.22
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
  package: montgomery-thm-1.21
  checksum: 0ff60cc84e2d7ba23cf384cf0dbd3cbac2564f94
}

hardware {
  import: thm
  package: montgomery-hardware-1.4
  checksum: d7ba08b9599c3ebdb1dabdd22844c3acdd2265c0
}

main {
  import: def
  import: thm
  import: hardware
}
