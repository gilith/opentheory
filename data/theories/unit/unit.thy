name: unit
version: 1.18
description: The unit type
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.11
  checksum: f26ed294f87998199389098ac823afcc12ab0b87
}

thm {
  import: def
  package: unit-thm-1.14
  checksum: c01774171295282d870e064854c3f7881a88b158
}

main {
  import: def
  import: thm
}
