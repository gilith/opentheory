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
}

choice {
  package: axiom-choice-1.8
}

infinity {
  package: axiom-infinity-1.12
}

main {
  import: extensionality
  import: choice
  import: infinity
}
