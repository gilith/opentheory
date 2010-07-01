name: hol-light-pair
version: 2009.8.24
description: HOL Light pair theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

pair-def {
  package: hol-light-pair-def-2009.8.24
}

pair-alt {
  import: pair-def
  package: hol-light-pair-alt-2009.8.24
}

pair-thm {
  import: pair-alt
  package: hol-light-pair-thm-2009.8.24
}

main {
  import: pair-alt
  import: pair-thm
}
