name: stream
version: 1.40
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
}

thm {
  import: def
  package: stream-thm-1.36
}

main {
  import: def
  import: thm
}
