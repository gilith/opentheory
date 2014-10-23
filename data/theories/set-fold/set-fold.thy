name: set-fold
version: 1.43
description: A fold operation on finite sets
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: set-finite
requires: set-thm
show: "Data.Bool"
show: "Number.Natural"
show: "Set"

def {
  package: set-fold-def-1.50
  checksum: 0c91a37b95ee36d01182da69c20a988e945a0026
}

thm {
  import: def
  package: set-fold-thm-1.39
  checksum: 564ec15ea879cdadd9e0b84c9a20c6c1813bbaa0
}

main {
  import: def
  import: thm
}
