name: hol-light-class-thm
version: 2009.8.24
description: HOL Light class theory theorems
author: Joe Hurd <joe@gilith.com>
license: HOLLight

class-eta-thm {
  package: hol-light-class-eta-thm-2009.8.24
}

class-select-thm {
  import: class-eta-thm
  package: hol-light-class-select-thm-2009.8.24
}

class-cond-thm {
  import: class-eta-thm
  import: class-select-thm
  package: hol-light-class-cond-thm-2009.8.24
}

class-skolem {
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  package: hol-light-class-skolem-2009.8.24
}

class-bool {
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  package: hol-light-class-bool-2009.8.24
}

main {
  import: class-eta-thm
  import: class-select-thm
  import: class-cond-thm
  import: class-skolem
  import: class-bool
}
