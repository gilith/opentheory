name: hol-light-thm
version: 2009.8.24
description: HOL Light theorems (no definitions or axioms).
author: Joe Hurd <joe@gilith.com>
license: HOLLight

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

arith-thm {
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
  package: hol-light-arith-thm-2009.8.24
}

wf-thm {
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
  import: arith-thm
  package: hol-light-wf-thm-2009.8.24
}

calc-num-thm {
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
  import: arith-thm
  import: wf-thm
  package: hol-light-calc-num-thm-2009.8.24
}

normalizer-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  package: hol-light-normalizer-thm-2009.8.24
}

grobner-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  package: hol-light-grobner-thm-2009.8.24
}

ind-types-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  package: hol-light-ind-types-thm-2009.8.24
}

list-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  package: hol-light-list-thm-2009.8.24
}

realax-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  package: hol-light-realax-thm-2009.8.24
}

calc-int-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  package: hol-light-calc-int-thm-2009.8.24
}

realarith-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  package: hol-light-realarith-thm-2009.8.24
}

real-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  package: hol-light-real-thm-2009.8.24
}

calc-rat-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  package: hol-light-calc-rat-thm-2009.8.24
}

int-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  package: hol-light-int-thm-2009.8.24
}

sets-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  import: int-thm
  package: hol-light-sets-thm-2009.8.24
}

iter-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  import: int-thm
  import: sets-thm
  package: hol-light-iter-thm-2009.8.24
}

cart-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  import: int-thm
  import: sets-thm
  import: iter-thm
  package: hol-light-cart-thm-2009.8.24
}

define-thm {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  import: int-thm
  import: sets-thm
  import: iter-thm
  import: cart-thm
  package: hol-light-define-thm-2009.8.24
}

main {
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
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  import: list-thm
  import: realax-thm
  import: calc-int-thm
  import: realarith-thm
  import: real-thm
  import: calc-rat-thm
  import: int-thm
  import: sets-thm
  import: iter-thm
  import: cart-thm
  import: define-thm
}
