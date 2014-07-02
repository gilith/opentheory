name: natural-prime-stream
version: 1.22
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
  package: natural-prime-stream-def-1.17
}

thm {
  import: def
  package: natural-prime-stream-thm-1.22
}

main {
  import: def
  import: thm
}
