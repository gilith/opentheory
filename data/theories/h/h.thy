name: h
version: 1.117
description: Memory safety for the H interface
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
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

def {
  package: h-def-1.117
  checksum: c3c3c2a880726002909bf9d3b2d8444996fcd700
}

thm {
  import: def
  package: h-thm-1.114
  checksum: 87f8b3b546970ee3516be161da3026b8c044f744
}

main {
  import: def
  import: thm
}
