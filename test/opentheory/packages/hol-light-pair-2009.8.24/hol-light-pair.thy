name: hol-light-pair
version: 2009.8.24
description: HOL Light pair theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: hol-light-pair-def-2009.8.24
}

require alt {
  import: def
  package: hol-light-pair-alt-2009.8.24
}

require thm {
  import: def
  import: alt
  package: hol-light-pair-thm-2009.8.24
}

theory {
  import def;
  import alt;
  import thm;
}
