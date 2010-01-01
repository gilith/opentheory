name: axiom-extensionality
version: 1.0
description: Axiom of extensionality
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: bool-def-1.0
}

require axiom {
  import: def
  package: hol-light-class-eta-axiom-2009.8.24
}

theory {
  import axiom;
}
