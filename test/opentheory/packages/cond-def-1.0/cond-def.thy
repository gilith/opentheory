name: cond-def
version: 1.0
description: Definition of the conditional
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool-thm {
  package: hol-light-bool-thm-2009.8.24
}

require tactics-thm {
  import: bool-thm
  package: hol-light-tactics-thm-2009.8.24
}

require simp-thm {
  import: bool-thm
  import: tactics-thm
  package: hol-light-simp-thm-2009.8.24
}

require theorems-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  package: hol-light-theorems-thm-2009.8.24
}

require ind-defs-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  package: hol-light-ind-defs-thm-2009.8.24
}

require class-eta-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  package: hol-light-class-eta-thm-2009.8.24
}

require class-select-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-eta-thm
  package: hol-light-class-select-thm-2009.8.24
}

require class-cond-def {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-eta-thm
  import: class-select-thm
  package: hol-light-class-cond-def-2009.8.24
}

require class-cond-alt {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-def
  package: hol-light-class-cond-alt-2009.8.24
}

theory {
  import class-cond-alt;
}
