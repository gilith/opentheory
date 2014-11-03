name: real
version: 1.60
description: The real numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.71
  checksum: d7b948b6f97c01585fdd6231ae6f481d1455ecf7
}

thm {
  import: def
  package: real-thm-1.48
  checksum: 8ff5b43d117ac361ecf6d8fc19378d621fe7935a
}

main {
  import: def
  import: thm
}
