name: natural-prime-stream
version: 1.27
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
  package: natural-prime-stream-thm-1.25
  checksum: 33eb28dc95baa039073a41d5a0327cebb2a2374c
}

main {
  import: def
  import: thm
}
