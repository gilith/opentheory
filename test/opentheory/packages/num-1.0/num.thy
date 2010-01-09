name: num
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require axiom-infinity {
  package: axiom-infinity-1.0
}

require num-def {
  import: axiom-infinity
  package: num-def-1.0
}

require numeral-def {
  import: axiom-infinity
  import: num-def
  package: numeral-def-1.0
}

theory {
  import num-def;
  import numeral-def;
}
