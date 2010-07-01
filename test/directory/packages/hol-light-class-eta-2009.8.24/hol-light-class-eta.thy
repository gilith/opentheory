name: hol-light-class-eta
version: 2009.8.24
description: HOL Light axiom of extensionality and derived theorems.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

class-eta-axiom {
  package: hol-light-class-eta-axiom-2009.8.24
}

class-eta-thm {
  import: class-eta-axiom
  package: hol-light-class-eta-thm-2009.8.24
}

main {
  import: class-eta-axiom
  import: class-eta-thm
}
