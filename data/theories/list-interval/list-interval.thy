name: list-interval
version: 1.58
description: The list interval function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-length
requires: list-map
requires: list-nth
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-interval-def-1.56
  checksum: 0fb5572fa52ba7c05f2164ca311a73d0295dac01
}

thm {
  import: def
  package: list-interval-thm-1.58
  checksum: 32359c12fd2369ff9daa9336acc7de0354b54412
}

main {
  import: def
  import: thm
}
