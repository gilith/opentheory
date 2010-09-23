name: bool
version: 1.0
description: Basic boolean theory
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"

bool-def {
  package: bool-def-1.0
}

bool-int {
  import: bool-def
  package: bool-int-1.0
}

axiom-extensionality {
  import: bool-def
  package: axiom-extensionality-1.0
}

bool-extensionality {
  import: bool-def
  import: bool-int
  import: axiom-extensionality
  package: bool-extensionality-1.0
}

axiom-choice {
  import: bool-def
  package: axiom-choice-1.0
}

bool-choice {
  import: bool-def
  import: bool-int
  import: axiom-extensionality
  import: bool-extensionality
  import: axiom-choice
  package: bool-choice-1.0
}

main {
  import: bool-def
  import: bool-int
  import: axiom-extensionality
  import: bool-extensionality
  import: axiom-choice
  import: bool-choice
}
