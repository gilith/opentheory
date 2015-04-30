name: bool
version: 1.36
description: Boolean operators and quantifiers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-def-1.11
}

int {
  import: def
  package: bool-int-1.17
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.9
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.12
}

axiom-choice {
  import: def
  package: axiom-choice-1.8
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.26
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
