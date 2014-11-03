name: function
version: 1.53
description: Function operators and combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.19
  checksum: 516c17dc59f626ec1471a5504c7e6b2d736e0545
}

thm {
  import: def
  package: function-thm-1.47
  checksum: 777ad161a0275d9e18299f073734c265cbec8022
}

main {
  import: def
  import: thm
}
