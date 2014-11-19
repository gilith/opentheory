name: natural-order-min-max
version: 1.39
description: Natural number min and max functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-order-def
requires: natural-order-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-order-min-max-def-1.29
  checksum: 756975b4923e6b7b8bca33930309ae17e5864a39
}

thm {
  import: def
  package: natural-order-min-max-thm-1.34
  checksum: ae5f32725c3af0e13f2b26dc48637ae48c30a530
}

main {
  import: def
  import: thm
}
