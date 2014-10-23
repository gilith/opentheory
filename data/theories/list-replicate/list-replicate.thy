name: list-replicate
version: 1.59
description: The list replicate function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-nth
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.50
  checksum: cc5966546cb0624b432693db21a23cddd4038db5
}

thm {
  import: def
  package: list-replicate-thm-1.59
  checksum: 661c4c7024993ddddc5847fbf8d30cc5baafd044
}

main {
  import: def
  import: thm
}
