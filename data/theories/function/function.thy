name: function
version: 1.54
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
  package: function-thm-1.48
  checksum: ec63820a81c0ee79660eae174d507340dbbcb5ad
}

main {
  import: def
  import: thm
}
