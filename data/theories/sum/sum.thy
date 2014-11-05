name: sum
version: 1.56
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
  package: sum-def-1.65
  checksum: c1149526a9d791ded296b5101ec17f59290170f8
}

main {
  import: def
}
