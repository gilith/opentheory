name: sum
version: 1.58
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
  package: sum-def-1.66
  checksum: bec2dbfd5a3a84dbfde582dd9ba3ede33787e8df
}

thm {
  import: def
  package: sum-thm-1.1
}

main {
  import: def
  import: thm
}
