name: hol-light-trivia-one
version: 2009.8.24
description: HOL Light definition and theorems about the unit type.
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: hol-light-trivia-one-def-2009.8.24
}

require alt {
  import: def
  package: hol-light-trivia-one-alt-2009.8.24
}

require thm {
  import: alt
  package: hol-light-trivia-one-thm-2009.8.24
}

theory {
  import alt;
  import thm;
}
