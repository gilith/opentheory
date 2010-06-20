name: hol-light
version: 2009.8.24
description: HOL Light theories
author: Joe Hurd <joe@gilith.com>
license: HOLLight

bool {
  package: hol-light-bool-2009.8.24
}

tactics {
  import: bool
  package: hol-light-tactics-2009.8.24
}

simp {
  import: bool
  import: tactics
  package: hol-light-simp-2009.8.24
}

theorems {
  import: bool
  import: tactics
  import: simp
  package: hol-light-theorems-2009.8.24
}

ind-defs {
  import: bool
  import: tactics
  import: simp
  import: theorems
  package: hol-light-ind-defs-2009.8.24
}

class {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  package: hol-light-class-2009.8.24
}

trivia {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  package: hol-light-trivia-2009.8.24
}

canon {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  package: hol-light-canon-2009.8.24
}

meson {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  package: hol-light-meson-2009.8.24
}

quot {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  package: hol-light-quot-2009.8.24
}

pair {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  package: hol-light-pair-2009.8.24
}

num {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  package: hol-light-num-2009.8.24
}

arith {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  package: hol-light-arith-2009.8.24
}

wf {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  package: hol-light-wf-2009.8.24
}

calc-num {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  package: hol-light-calc-num-2009.8.24
}

normalizer {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  package: hol-light-normalizer-2009.8.24
}

grobner {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  package: hol-light-grobner-2009.8.24
}

ind-types {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  package: hol-light-ind-types-2009.8.24
}

list {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  package: hol-light-list-2009.8.24
}

realax {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  package: hol-light-realax-2009.8.24
}

calc-int {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  package: hol-light-calc-int-2009.8.24
}

realarith {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  package: hol-light-realarith-2009.8.24
}

real {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  package: hol-light-real-2009.8.24
}

calc-rat {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  package: hol-light-calc-rat-2009.8.24
}

int {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  package: hol-light-int-2009.8.24
}

sets {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  import: int
  package: hol-light-sets-2009.8.24
}

iter {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  import: int
  import: sets
  package: hol-light-iter-2009.8.24
}

cart {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  import: int
  import: sets
  import: iter
  package: hol-light-cart-2009.8.24
}

define {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  import: int
  import: sets
  import: iter
  import: cart
  package: hol-light-define-2009.8.24
}

main {
  import: bool
  import: tactics
  import: simp
  import: ind-defs
  import: class
  import: trivia
  import: canon
  import: meson
  import: quot
  import: pair
  import: num
  import: arith
  import: wf
  import: calc-num
  import: normalizer
  import: grobner
  import: ind-types
  import: list
  import: realax
  import: calc-int
  import: realarith
  import: real
  import: calc-rat
  import: int
  import: sets
  import: iter
  import: cart
  import: define
}
