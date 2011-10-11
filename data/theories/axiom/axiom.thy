name: axiom
version: 1.5
description: Standard axioms
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.4
}

choice {
  package: axiom-choice-1.4
}

infinity {
  package: axiom-infinity-1.3
}

main {
  import: extensionality
  import: choice
  import: infinity
}
