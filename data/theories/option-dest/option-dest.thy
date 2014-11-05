name: option-dest
version: 1.55
description: Option type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.60
  checksum: 320c42ccb9cf0e6c6681e5e9da3f5cb8fe2c8b78
}

thm {
  import: def
  package: option-dest-thm-1.15
  checksum: 98c003e0912920e764ac03c4277131792b1e78e4
}

main {
  import: def
  import: thm
}
