name: bool
version: 1.2
description: Basic boolean theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-def-1.1
}

int {
  import: def
  package: bool-int-1.2
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.0
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.0
}

axiom-choice {
  import: def
  package: axiom-choice-1.0
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.0
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
