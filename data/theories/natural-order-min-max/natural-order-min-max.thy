name: natural-order-min-max
version: 1.38
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
  package: natural-order-min-max-thm-1.33
  checksum: 3d9b9dbff1acb90e52cca019efdd0edac30e6109
}

main {
  import: def
  import: thm
}
