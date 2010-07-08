name: bool
version: 1.0
description: Basic boolean theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"

bool-true-def {
  package: bool-true-def-1.0
}

bool-true-thm {
  import: bool-true-def
  package: bool-true-thm-1.0
}

bool-and-def {
  import: bool-true-def
  package: bool-and-def-1.0
}

bool-imp-def {
  import: bool-and-def
  package: bool-imp-def-1.0
}

bool-forall-def {
  import: bool-true-def
  package: bool-forall-def-1.0
}

bool-exists-def {
  import: bool-imp-def
  import: bool-forall-def
  package: bool-exists-def-1.0
}

bool-or-def {
  import: bool-imp-def
  import: bool-forall-def
  package: bool-or-def-1.0
}

bool-false-def {
  import: bool-forall-def
  package: bool-false-def-1.0
}

bool-not-def {
  import: bool-imp-def
  import: bool-false-def
  package: bool-not-def-1.0
}

bool-exists-unique-def {
  import: bool-and-def
  import: bool-imp-def
  import: bool-forall-def
  import: bool-exists-def
  package: bool-exists-unique-def-1.0
}

axiom-extensionality {
  package: axiom-extensionality-1.0
}

axiom-choice {
  package: axiom-choice-1.0
}

bool-cond-def {
  import: axiom-extensionality
  import: axiom-choice
  package: bool-cond-def-1.0
}

main {
  import: bool-true-def
  import: bool-true-thm
  import: bool-and-def
  import: bool-imp-def
  import: bool-forall-def
  import: bool-exists-def
  import: bool-or-def
  import: bool-false-def
  import: bool-not-def
  import: bool-exists-unique-def
  import: axiom-extensionality
  import: axiom-choice
  import: bool-cond-def
}
