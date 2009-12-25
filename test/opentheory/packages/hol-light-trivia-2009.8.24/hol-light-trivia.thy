name: hol-light-trivia
version: 2009.8.24
description: HOL Light trivia theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require comb {
  package: hol-light-trivia-comb-2009.8.24
}

require one {
  import: comb
  package: hol-light-trivia-one-2009.8.24
}

theory {
  import comb;
  import one;
}
