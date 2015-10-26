name: map-reduce-bit3x3
version: 1.7
description: The map reduce 3x3 bit matrix example
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art

def {
  package: map-reduce-bit3x3-def-1.1
}

sat {
  package: map-reduce-bit3x3-sat-1.4
}

thm {
  import: def
  import: sat
  package: map-reduce-bit3x3-thm-1.1
}

main {
  import: def
  import: thm
}
