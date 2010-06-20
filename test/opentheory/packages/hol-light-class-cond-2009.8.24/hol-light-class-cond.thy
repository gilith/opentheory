name: hol-light-class-cond
version: 2009.8.24
description: HOL Light definition and theorems about the conditional.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

class-cond-def {
  package: hol-light-class-cond-def-2009.8.24
}

class-cond-alt {
  import: class-cond-def
  package: hol-light-class-cond-alt-2009.8.24
}

class-cond-thm {
  import: class-cond-alt
  package: hol-light-class-cond-thm-2009.8.24
}

main {
  import: class-cond-alt
  import: class-cond-thm
}
