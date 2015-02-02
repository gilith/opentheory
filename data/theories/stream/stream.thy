name: stream
version: 1.39
description: Stream types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Set"
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: stream-def-1.34
  checksum: d377adcba0edfa929947b0219c307834dc75128c
}

thm {
  import: def
  package: stream-thm-1.36
  checksum: 8d10919971d393658269820281e86c8d8afcc41a
}

main {
  import: def
  import: thm
}
