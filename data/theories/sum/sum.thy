name: sum
version: 1.29
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
  package: sum-def-1.35
}

main {
  import: def
}
