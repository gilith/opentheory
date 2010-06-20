name: hol-light-trivia-one
version: 2009.8.24
description: HOL Light definition and theorems about the unit type.
author: Joe Hurd <joe@gilith.com>
license: HOLLight

trivia-one-def {
  package: hol-light-trivia-one-def-2009.8.24
}

trivia-one-alt {
  import: trivia-one-def
  package: hol-light-trivia-one-alt-2009.8.24
}

trivia-one-thm {
  import: trivia-one-alt
  package: hol-light-trivia-one-thm-2009.8.24
}

main {
  import: trivia-one-alt
  import: trivia-one-thm
}
