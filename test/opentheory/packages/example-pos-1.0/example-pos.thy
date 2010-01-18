name: example-pos
version: 1.0
description: Constructing the whole set of numbers from the positive subset
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require hol-light-thm {
  package: hol-light-thm-2009.8.24
}

require hol-light-example-pos {
  import: hol-light-thm
  package: hol-light-example-pos-2009.8.24
}

theory {
  import hol-light-example-pos;
}
