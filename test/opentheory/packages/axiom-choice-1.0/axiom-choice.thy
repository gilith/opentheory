name: axiom-choice
version: 1.0
description: Axiom of choice
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool-def {
  package: bool-def-1.0
}

require axiom {
  import: bool-def
  package: hol-light-class-select-axiom-2009.8.24
}

theory {
  import axiom;
}
