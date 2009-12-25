name: hol-light-bool
version: 2009.8.24
description: HOL Light bool theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require def {
  package: hol-light-bool-def-2009.8.24
}

require rule {
  import: def
  package: hol-light-bool-rule-2009.8.24
}

theory {
  import def;
  import rule;
}
