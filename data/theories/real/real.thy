name: real
version: 1.61
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
  package: real-def-1.72
  checksum: 97223c45e4035fa0a3f9f4f2f3196ab9bfacd2be
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
