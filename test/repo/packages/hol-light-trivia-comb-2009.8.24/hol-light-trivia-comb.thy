name: hol-light-trivia-comb
version: 2009.8.24
description: HOL Light definition and theorems about combinators.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

trivia-comb-def {
  package: hol-light-trivia-comb-def-2009.8.24
}

trivia-comb-thm {
  import: trivia-comb-def
  package: hol-light-trivia-comb-thm-2009.8.24
}

main {
  import: trivia-comb-def
  import: trivia-comb-thm
}
