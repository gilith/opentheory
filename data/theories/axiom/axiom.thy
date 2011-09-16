name: axiom
version: 1.3
description: Standard axioms
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.3
}

choice {
  package: axiom-choice-1.3
}

infinity {
  package: axiom-infinity-1.1
}

main {
  import: extensionality
  import: choice
  import: infinity
}
