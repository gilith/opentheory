name: hol-light-class-select
version: 2009.8.24
description: HOL Light axiom of choice and derived theorems.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require axiom {
  package: hol-light-class-select-axiom-2009.8.24
}

require thm {
  import: axiom
  package: hol-light-class-select-thm-2009.8.24
}

theory {
  import axiom;
  import thm;
}
