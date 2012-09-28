name: axiom
version: 1.11
description: Standard axioms
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.8
}

choice {
  package: axiom-choice-1.7
}

infinity {
  package: axiom-infinity-1.7
}

main {
  import: extensionality
  import: choice
  import: infinity
}
