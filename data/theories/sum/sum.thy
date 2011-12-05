name: sum
version: 1.28
description: Sum types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"

def {
  package: sum-def-1.34
}

main {
  import: def
}
