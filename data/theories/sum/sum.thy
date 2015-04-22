name: sum
version: 1.60
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
  package: sum-def-1.67
}

thm {
  import: def
  package: sum-thm-1.2
}

main {
  import: def
  import: thm
}
