name: natural-order
version: 1.51
description: Natural number orderings
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-def
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-def-1.33
  checksum: 293458135a73543b3370c1f624a4b6f25e0ff520
}

thm {
  import: def
  package: natural-order-thm-1.41
  checksum: 5d29867a5506bb2cf58c3530644e21d00f6f5256
}

min-max {
  import: def
  import: thm
  package: natural-order-min-max-1.39
  checksum: 90e50fa99202de1e8908cc95da625c474a91a45d
}

main {
  import: def
  import: thm
  import: min-max
}
