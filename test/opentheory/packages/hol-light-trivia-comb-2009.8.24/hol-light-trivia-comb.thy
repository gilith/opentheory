name: hol-light-trivia-comb
version: 2009.8.24
description: HOL Light definition and theorems about combinators.
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: hol-light-trivia-comb-def-2009.8.24
}

require thm {
  import: def
  package: hol-light-trivia-comb-thm-2009.8.24
}

theory {
  import def;
  import thm;
}
