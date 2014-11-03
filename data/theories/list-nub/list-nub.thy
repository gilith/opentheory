name: list-nub
version: 1.56
description: The list nub function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-length
requires: list-reverse
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nub-def-1.53
  checksum: 5337d8ae920d49b7dfb804faa5e744579386c186
}

thm {
  import: def
  package: list-nub-thm-1.57
  checksum: 2d9c503e0eb2de348abda0002853088472fa8963
}

main {
  import: def
  import: thm
}
