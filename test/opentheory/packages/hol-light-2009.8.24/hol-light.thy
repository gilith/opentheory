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

theory {
  import bool;
  import tactics;
  import simp;
  import ind-defs;
  import class;
  import trivia;
}