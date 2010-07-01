name: hol-light-trivia
version: 2009.8.24
description: HOL Light trivia theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

trivia-comb {
  package: hol-light-trivia-comb-2009.8.24
}

trivia-one {
  import: trivia-comb
  package: hol-light-trivia-one-2009.8.24
}

main {
  import: trivia-comb
  import: trivia-one
}
