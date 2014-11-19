name: option-dest
version: 1.56
description: Option type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.61
  checksum: e188551ac0339cac419cb13ebfc102dc6b876402
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
