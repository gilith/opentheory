name: hol-light-thm
version: 2009.8.24
description: HOL Light theorems (no definitions or axioms).
author: Joe Hurd <joe@gilith.com>
license: HOLLight

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
  package: hol-light-num-thm-2009.8.24
}

require arith-thm {
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
  import: num-thm
  package: hol-light-arith-thm-2009.8.24
}

require wf-thm {
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
  import: num-thm
  import: arith-thm
  package: hol-light-wf-thm-2009.8.24
}

require calc-num-thm {
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
  import: num-thm
  import: arith-thm
  import: wf-thm
  package: hol-light-calc-num-thm-2009.8.24
}

require normalizer-thm {
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
  import: num-thm
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  package: hol-light-normalizer-thm-2009.8.24
}

require grobner-thm {
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
  import: num-thm
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  package: hol-light-grobner-thm-2009.8.24
}

require ind-types-thm {
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
  import: num-thm
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  package: hol-light-ind-types-thm-2009.8.24
}

require list-thm {
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
  import: num-thm
  import: arith-thm
  import: wf-thm
  import: calc-num-thm
  import: normalizer-thm
  import: grobner-thm
  import: ind-types-thm
  package: hol-light-list-thm-2009.8.24
}

require realax-thm {
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

require calc-int-thm {
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

require realarith-thm {
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

require real-thm {
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

require calc-rat-thm {
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

require int-thm {
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

require sets-thm {
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

require iter-thm {
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

require cart-thm {
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

require define-thm {
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

theory {
  import bool-thm;
  import tactics-thm;
  import simp-thm;
  import theorems-thm;
  import ind-defs-thm;
  import class-thm;
  import trivia-thm;
  import canon-thm;
  import meson-thm;
  import quot-thm;
  import pair-thm;
  import num-thm;
  import arith-thm;
  import wf-thm;
  import calc-num-thm;
  import normalizer-thm;
  import grobner-thm;
  import ind-types-thm;
  import list-thm;
  import realax-thm;
  import calc-int-thm;
  import realarith-thm;
  import real-thm;
  import calc-rat-thm;
  import int-thm;
  import sets-thm;
  import iter-thm;
  import cart-thm;
  import define-thm;
}
