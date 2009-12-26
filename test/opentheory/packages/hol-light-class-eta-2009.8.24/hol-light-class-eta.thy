name: hol-light-class-eta
version: 2009.8.24
description: HOL Light axiom of extensionality and derived theorems.
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require axiom {
  package: hol-light-class-eta-axiom-2009.8.24
}

require thm {
  import: axiom
  package: hol-light-class-eta-thm-2009.8.24
}

theory {
  import axiom;
  import thm;
}
