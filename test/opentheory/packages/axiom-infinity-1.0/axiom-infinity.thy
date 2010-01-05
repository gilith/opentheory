name: axiom-infinity
version: 1.0
description: Axiom of infinity
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool-def {
  package: bool-def-1.0
}

require function-def {
  import: bool-def
  package: function-def-1.0
}

require axiom {
  import: bool-def
  import: function-def
  package: hol-light-num-infinity-2009.8.24
}

theory {
  import axiom;
}
