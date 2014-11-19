name: function
version: 1.55
description: Function operators and combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.20
  checksum: 80cdbf8f0b994aa8376b1c615f1917790666025e
}

thm {
  import: def
  package: function-thm-1.49
  checksum: c4b088167b5134dce5c967c7904133bef5f15bb3
}

main {
  import: def
  import: thm
}
