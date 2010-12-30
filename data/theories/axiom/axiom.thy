name: axiom
version: 1.0
description: Basic axioms
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

extensionality {
  package: axiom-extensionality-1.0
}

choice {
  package: axiom-choice-1.0
}

infinity {
  package: axiom-infinity-1.0
}

main {
  import: extensionality
  import: choice
  import: infinity
}
