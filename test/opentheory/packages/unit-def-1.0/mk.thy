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

require class-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  package: hol-light-class-thm-2009.8.24
}

require trivia-comb-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  package: hol-light-trivia-comb-thm-2009.8.24
}

require trivia-one-def {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-comb-thm
  package: hol-light-trivia-one-def-2009.8.24
}

require trivia-one-alt {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-comb-thm
  import: trivia-one-def
  package: hol-light-trivia-one-alt-2009.8.24
}

theory {
  import trivia-one-alt;
}
