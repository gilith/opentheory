name: sum
version: 1.46
description: Sum types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"

def {
  package: sum-def-1.55
}

main {
  import: def
}
