name: axiom
version: 1.9
description: Standard axioms
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.6
}

choice {
  package: axiom-choice-1.5
}

infinity {
  package: axiom-infinity-1.5
}

main {
  import: extensionality
  import: choice
  import: infinity
}
