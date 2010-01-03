name: pair-def
version: 1.0
description: Definition of the product type
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool-rule {
  package: hol-light-bool-rule-2009.8.24
}

require tactics {
  import: bool-rule
  package: hol-light-tactics-2009.8.24
}

require simp {
  import: bool-rule
  import: tactics
  package: hol-light-simp-2009.8.24
}

require theorems {
  import: bool-rule
  import: tactics
  import: simp
  package: hol-light-theorems-2009.8.24
}

require ind-defs {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  package: hol-light-ind-defs-2009.8.24
}

require class-eta-thm {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  package: hol-light-class-eta-thm-2009.8.24
}

require class-select-thm {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  package: hol-light-class-select-thm-2009.8.24
}

require class-cond-thm {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  package: hol-light-class-cond-thm-2009.8.24
}

require class-skolem {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  package: hol-light-class-skolem-2009.8.24
}

require class-bool {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  package: hol-light-class-bool-2009.8.24
}

require trivia-comb {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  package: hol-light-trivia-comb-2009.8.24
}

require trivia-one-thm {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  package: hol-light-trivia-one-thm-2009.8.24
}

require canon {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  import: trivia-one-thm
  package: hol-light-canon-2009.8.24
}

require meson {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  import: trivia-one-thm
  import: canon
  package: hol-light-meson-2009.8.24
}

require quot {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  import: trivia-one-thm
  import: canon
  import: meson
  package: hol-light-quot-2009.8.24
}

require def {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  import: trivia-one-thm
  import: canon
  import: meson
  import: quot
  package: hol-light-pair-def-2009.8.24
}

require alt {
  import: bool-rule
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
  import: trivia-comb
  import: trivia-one-thm
  import: canon
  import: meson
  import: quot
  import: def
  package: hol-light-pair-alt-2009.8.24
}

theory {
  import alt;
}
