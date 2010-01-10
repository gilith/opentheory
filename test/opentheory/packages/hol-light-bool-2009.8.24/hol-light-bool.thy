name: hol-light-bool
version: 2009.8.24
description: HOL Light bool theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require def {
  package: hol-light-bool-def-2009.8.24
}

require thm {
  import: def
  package: hol-light-bool-thm-2009.8.24
}

theory {
  import def;
  import thm;
}
