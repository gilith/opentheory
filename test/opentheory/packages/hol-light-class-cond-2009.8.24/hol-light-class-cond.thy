name: hol-light-class-cond
version: 2009.8.24
description: HOL Light definition and theorems about the conditional.
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: hol-light-class-cond-def-2009.8.24
}

require thm {
  import: def
  package: hol-light-class-cond-thm-2009.8.24
}

theory {
  import def;
  import thm;
}
