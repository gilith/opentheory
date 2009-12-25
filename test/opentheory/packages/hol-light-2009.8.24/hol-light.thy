name: hol-light
version: 2009.8.24
description: HOL Light theories
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool {
  package: hol-light-bool-2009.8.24
}

require tactics {
  import: bool
  package: hol-light-tactics-2009.8.24
}

require simp {
  import: bool
  import: tactics
  package: hol-light-simp-2009.8.24
}

require theorems {
  import: bool
  import: tactics
  import: simp
  package: hol-light-theorems-2009.8.24
}

require ind-defs {
  import: bool
  import: tactics
  import: simp
  import: theorems
  package: hol-light-ind-defs-2009.8.24
}

require class {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  package: hol-light-class-2009.8.24
}

require trivia {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  package: hol-light-trivia-2009.8.24
}

require canon {
  import: bool
  import: tactics
  import: simp
  import: theorems
  import: ind-defs
  import: class
  import: trivia
  package: hol-light-canon-2009.8.24
}

require meson {
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

require quot {
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

require pair {
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

require num {
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

require arith {
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

require wf {
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

require calc-num {
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

require normalizer {
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

require grobner {
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

require ind-types {
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

require list {
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

require realax {
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

require calc-int {
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

require realarith {
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

require real {
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

require calc-rat {
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

require int {
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

require sets {
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

require iter {
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

require cart {
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

require define {
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

theory {
  import bool;
  import tactics;
  import simp;
  import ind-defs;
  import class;
  import trivia;
  import canon;
  import meson;
  import quot;
  import pair;
  import num;
  import arith;
  import wf;
  import calc-num;
  import normalizer;
  import grobner;
  import ind-types;
  import list;
  import realax;
  import calc-int;
  import realarith;
  import real;
  import calc-rat;
  import int;
  import sets;
  import iter;
  import cart;
  import define;
}
