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

class-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  package: hol-light-class-thm-2009.8.24
}

canon-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  package: hol-light-canon-thm-2009.8.24
}

meson-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: canon-thm
  package: hol-light-meson-thm-2009.8.24
}

quot-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: canon-thm
  import: meson-thm
  package: hol-light-quot-thm-2009.8.24
}

pair-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  package: hol-light-pair-thm-2009.8.24
}

num-thm {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  import: pair-thm
  package: hol-light-num-thm-2009.8.24
}

arith-pre-def {
  import: bool-thm
  import: tactics-thm
  import: simp-thm
  import: theorems-thm
  import: ind-defs-thm
  import: class-thm
  import: canon-thm
  import: meson-thm
  import: quot-thm
  import: pair-thm
  import: num-thm
  package: hol-light-arith-pre-def-2009.8.24
}

main {
  import: arith-pre-def
}
