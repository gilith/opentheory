name: sum
version: 1.12
description: Sum types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
show: "Data.Bool"
show: "Data.Sum"
show: "Number.Natural"

def {
  package: sum-def-1.12
}

main {
  import: def
}
