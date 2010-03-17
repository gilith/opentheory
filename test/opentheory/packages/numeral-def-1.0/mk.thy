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

require trivia-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  package: hol-light-trivia-thm-2009.8.24
}

require canon-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  package: hol-light-canon-thm-2009.8.24
}

require meson-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  package: hol-light-meson-thm-2009.8.24
}

require quot-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  import: meson-thm
  package: hol-light-quot-thm-2009.8.24
}

require pair-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  package: hol-light-pair-thm-2009.8.24
}

require num-numeral {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  import: pair-thm
  package: hol-light-num-numeral-2009.8.24
}

require num-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  import: pair-thm
  import: num-numeral
  package: hol-light-num-thm-2009.8.24
}

require num-bit {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: trivia-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  import: pair-thm
  import: num-numeral
  import: num-thm
  package: hol-light-num-bit-2009.8.24
}

theory {
  import num-numeral;
  import num-bit;
}
