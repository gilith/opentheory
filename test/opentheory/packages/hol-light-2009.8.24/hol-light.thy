name: hol-light
version: 2009.8.24
description: HOL Light theories
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require bool {
  package: hol-light-bool-2009.8.24
}

require tactics {
  require: bool
  package: hol-light-tactics-2009.8.24
}

require simp {
  require: bool
  require: tactics
  package: hol-light-simp-2009.8.24
}

require theorems {
  require: bool
  require: tactics
  require: simp
  package: hol-light-theorems-2009.8.24
}

require ind-defs {
  require: bool
  require: tactics
  require: simp
  require: theorems
  package: hol-light-ind-defs-2009.8.24
}

require class {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  package: hol-light-class-2009.8.24
}

require trivia {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  package: hol-light-trivia-2009.8.24
}

require canon {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  package: hol-light-canon-2009.8.24
}

require meson {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  package: hol-light-meson-2009.8.24
}

require quot {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  package: hol-light-quot-2009.8.24
}

require pair {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  package: hol-light-pair-2009.8.24
}

require num {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  package: hol-light-num-2009.8.24
}

require arith {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  package: hol-light-arith-2009.8.24
}

require wf {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  package: hol-light-wf-2009.8.24
}

require calc-num {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  package: hol-light-calc-num-2009.8.24
}

require normalizer {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  package: hol-light-normalizer-2009.8.24
}

require grobner {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  package: hol-light-grobner-2009.8.24
}

require ind-types {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  package: hol-light-ind-types-2009.8.24
}

require list {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  package: hol-light-list-2009.8.24
}

require realax {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  package: hol-light-realax-2009.8.24
}

require calc-int {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  package: hol-light-calc-int-2009.8.24
}

require realarith {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  package: hol-light-realarith-2009.8.24
}

require real {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  package: hol-light-real-2009.8.24
}

require calc-rat {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  package: hol-light-calc-rat-2009.8.24
}

require int {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  require: calc-rat
  package: hol-light-int-2009.8.24
}

require sets {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  require: calc-rat
  require: int
  package: hol-light-sets-2009.8.24
}

require iter {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  require: calc-rat
  require: int
  require: sets
  package: hol-light-iter-2009.8.24
}

require cart {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  require: calc-rat
  require: int
  require: sets
  require: iter
  package: hol-light-cart-2009.8.24
}

require define {
  require: bool
  require: tactics
  require: simp
  require: theorems
  require: ind-defs
  require: class
  require: trivia
  require: canon
  require: meson
  require: quot
  require: pair
  require: num
  require: arith
  require: wf
  require: calc-num
  require: normalizer
  require: grobner
  require: ind-types
  require: list
  require: realax
  require: calc-int
  require: realarith
  require: real
  require: calc-rat
  require: int
  require: sets
  require: iter
  require: cart
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
