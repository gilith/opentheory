name: bool
version: 1.9
description: Standard boolean theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-def-1.5
}

int {
  import: def
  package: bool-int-1.4
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.4
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.4
}

axiom-choice {
  import: def
  package: axiom-choice-1.4
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.4
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
