name: set-fold
version: 1.45
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
  package: set-fold-def-1.52
  checksum: 21e9e460b1c78e6db7cb01ef6e9c8e5bd3c5f68e
}

thm {
  import: def
  package: set-fold-thm-1.41
  checksum: ff1d6518ab3afb86a52b15a408eada0b79d574e6
}

main {
  import: def
  import: thm
}
