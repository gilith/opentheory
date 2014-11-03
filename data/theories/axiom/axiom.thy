name: axiom
version: 1.16
description: Standard axioms
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.9
  checksum: 6e9a7c8e10c21e54d5f3285cb11e65de99c9542d
}

choice {
  package: axiom-choice-1.8
  checksum: 2178247da99e65a9a2e0a1093adbff512d1539d9
}

infinity {
  package: axiom-infinity-1.12
  checksum: 53e364be097eb2d2284001aa97935dcee31a969c
}

main {
  import: extensionality
  import: choice
  import: infinity
}
