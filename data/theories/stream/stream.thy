name: stream
version: 1.46
description: Infinite stream types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
homepage: http://opentheory.gilith.com/?pkg=stream
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Set"
haskell-category: List
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: stream-def-1.36
}

thm {
  import: def
  package: stream-thm-1.39
}

main {
  import: def
  import: thm
}
