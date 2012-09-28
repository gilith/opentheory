name: bool
version: 1.29
description: Boolean operators and quantifiers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-def-1.10
}

int {
  import: def
  package: bool-int-1.17
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.8
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.10
}

axiom-choice {
  import: def
  package: axiom-choice-1.7
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.19
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
