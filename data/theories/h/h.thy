name: h
version: 1.125
description: Memory safety for the H interface
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
requires: natural-bits
requires: word10
requires: word12
show: "Data.Bool"
show: "Data.Byte"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word10"
show: "Data.Word12"
show: "Function"
show: "Number.Natural"
show: "Set"
show: "System.H"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art

def {
  package: h-def-1.117
}

thm {
  import: def
  package: h-thm-1.119
}

main {
  import: def
  import: thm
}
