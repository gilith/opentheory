name: sum
version: 1.53
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
  package: sum-def-1.62
  checksum: a7abc28f6857f4accbf27c1c57daa270df86e774
}

main {
  import: def
}
