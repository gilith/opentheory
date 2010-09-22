name: bool-choice
version: 1.0
description: Basic boolean theory relying on the axiom of choice
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"

bool-choice-exists {
  package: bool-choice-exists-1.0
}

# bool-def-cond {
#   import: axiom-extensionality
#   import: axiom-choice
#   package: bool-def-cond-1.0
# }

main {
  import: bool-choice-exists
}
