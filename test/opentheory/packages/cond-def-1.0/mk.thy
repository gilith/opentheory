bool-thm {
  package: hol-light-bool-thm-2009.8.24
}

tactics-thm {
  import: bool-thm
  package: hol-light-tactics-thm-2009.8.24
}

simp-thm {
  import: bool-thm
  import: tactics-thm
  package: hol-light-simp-thm-2009.8.24
}

theorems-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  package: hol-light-theorems-thm-2009.8.24
}

ind-defs-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  package: hol-light-ind-defs-thm-2009.8.24
}

class-eta-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  package: hol-light-class-eta-thm-2009.8.24
}

class-select-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-eta-thm
  package: hol-light-class-select-thm-2009.8.24
}

class-cond-def {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-eta-thm
  import: class-select-thm
  package: hol-light-class-cond-def-2009.8.24
}

class-cond-alt {
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

main {
  import: class-cond-alt
}
