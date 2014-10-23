name: h
version: 1.114
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
  package: h-def-1.114
  checksum: 488d2abce8f87fe0ce6b23225954f68946208738
}

thm {
  import: def
  package: h-thm-1.112
  checksum: cb697aba918dcd2311d788a46a58192c2e1a6749
}

main {
  import: def
  import: thm
}
