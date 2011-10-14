name: bool
version: 1.12
description: Boolean operators and quantifiers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: empty
show: "Data.Bool"

def {
  package: bool-def-1.6
}

int {
  import: def
  package: bool-int-1.4
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.5
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.5
}

axiom-choice {
  import: def
  package: axiom-choice-1.5
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
