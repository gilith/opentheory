name: sum
version: 1.59
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
  checksum: 70559fe2cff865e12eed2572bf2168296e3be301
}

thm {
  import: def
  package: sum-thm-1.1
  checksum: 01fd163ede0704e34ff00760c9bb8182f4e51762
}

main {
  import: def
  import: thm
}
