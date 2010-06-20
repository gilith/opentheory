name: hol-light-class-select
version: 2009.8.24
description: HOL Light axiom of choice and derived theorems.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

class-select-axiom {
  package: hol-light-class-select-axiom-2009.8.24
}

class-select-thm {
  import: class-select-axiom
  package: hol-light-class-select-thm-2009.8.24
}

main {
  import: class-select-axiom
  import: class-select-thm
}
