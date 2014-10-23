name: list-zip
version: 1.24
description: The list zip function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: list-zip-def-1.18
  checksum: 52f5a4bbad584863e117f6a5ba8c0b770e5582a0
}

thm {
  import: def
  package: list-zip-thm-1.22
  checksum: 51bcb0e4bcaa09fd8f4c50708f42ecdc2d29ef6a
}

main {
  import: def
  import: thm
}
