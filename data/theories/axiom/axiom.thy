name: axiom
version: 1.14
description: Standard axioms
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.8
  checksum: a03fa11c985e29adbd28e95405b3fe8c47c0bad5
}

choice {
  package: axiom-choice-1.7
  checksum: 8b3cfcf9cc420a78142dae227f28d1497cc8e76d
}

infinity {
  package: axiom-infinity-1.10
  checksum: 3e38e591a4adc6637b654bedc8fc2b81b1f0dacc
}

main {
  import: extensionality
  import: choice
  import: infinity
}
