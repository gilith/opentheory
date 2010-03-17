name: bool
version: 1.0
description: Basic boolean theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"

require bool-def {
  package: bool-def-1.0
}

require axiom-extensionality {
  import: bool-def
  package: axiom-extensionality-1.0
}

require axiom-choice {
  import: bool-def
  package: axiom-choice-1.0
}

require cond-def {
  import: bool-def
  import: axiom-extensionality
  import: axiom-choice
  package: cond-def-1.0
}

theory {
  import bool-def;
  import axiom-extensionality;
  import axiom-choice;
  import cond-def;
}
