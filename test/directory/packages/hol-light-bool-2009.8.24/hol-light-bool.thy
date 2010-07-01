name: hol-light-bool
version: 2009.8.24
description: HOL Light bool theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

bool-def {
  package: hol-light-bool-def-2009.8.24
}

bool-thm {
  import: bool-def
  package: hol-light-bool-thm-2009.8.24
}

main {
  import: bool-def
  import: bool-thm
}
