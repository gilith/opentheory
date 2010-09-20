name: bool
version: 1.0
description: Basic boolean theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"

bool-def {
  package: bool-def-1.0
}

bool-int {
  import: bool-def
  package: bool-int-1.0
}

# axiom-extensionality {
#   package: axiom-extensionality-1.0
# }

# axiom-choice {
#   package: axiom-choice-1.0
# }

# bool-cond-def {
#   import: axiom-extensionality
#   import: axiom-choice
#   package: bool-cond-def-1.0
# }

main {
  import: bool-def
  import: bool-int
#   import: bool-and-def
#   import: bool-imp-def
#   import: bool-forall-def
#   import: bool-exists-def
#   import: bool-or-def
#   import: bool-false-def
#   import: bool-not-def
#   import: bool-exists-unique-def
#   import: axiom-extensionality
#   import: axiom-choice
#   import: bool-cond-def
}
