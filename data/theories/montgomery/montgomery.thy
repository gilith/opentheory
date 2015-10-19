name: montgomery
version: 1.29
description: Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2015-10-18
requires: base
requires: hardware
requires: natural-bits
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Hardware"
show: "Number.Natural"
show: "Set"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art

def {
  package: montgomery-def-1.10
}

thm {
  import: def
  package: montgomery-thm-1.23
}

hardware {
  import: thm
  package: montgomery-hardware-1.7
}

main {
  import: def
  import: thm
  import: hardware
}
