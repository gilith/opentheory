name: list-fold
version: 1.27
description: List fold operations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list-append
requires: list-def
requires: list-length
requires: list-reverse
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"

def {
  package: list-fold-def-1.26
  checksum: f2bac0cb26c9abac343e1ea40570f516bd0d007b
}

thm {
  import: def
  package: list-fold-thm-1.27
  checksum: 76c9102db325d02371fa4f360fde6da7d52ed109
}

main {
  import: def
  import: thm
}
