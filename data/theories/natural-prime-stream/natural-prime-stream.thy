name: natural-prime-stream
version: 1.28
description: The ordered stream of all prime numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
requires: natural-prime-thm
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Stream"
show: "Number.Natural"

def {
  package: natural-prime-stream-def-1.22
  checksum: c0bfa33424ba1e4ed8e054b75972429903f73294
}

thm {
  import: def
  package: natural-prime-stream-thm-1.26
  checksum: 7784e3c3d1560fb535219447c78cd1fcb40c367c
}

main {
  import: def
  import: thm
}
