name: set-fold
version: 1.46
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
  package: set-fold-def-1.53
  checksum: 9fe7a20d5ee3c524740cac61f2deb1f380403d9a
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
