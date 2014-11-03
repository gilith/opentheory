name: sum
version: 1.55
description: Sum types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"

def {
  package: sum-def-1.64
  checksum: c034e0ab572f4da8f677502133d177fe3ba1990f
}

main {
  import: def
}
