name: hol-light-class-thm
version: 2009.8.24
description: HOL Light class theory theorems
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require eta-thm {
  package: hol-light-class-eta-thm-2009.8.24
}

require select-thm {
  import: eta-thm
  package: hol-light-class-select-thm-2009.8.24
}

require cond-thm {
  import: eta-thm
  import: select-thm
  package: hol-light-class-cond-thm-2009.8.24
}

require skolem {
  import: eta-thm
  import: select-thm
  import: cond-thm
  package: hol-light-class-skolem-2009.8.24
}

require bool {
  import: eta-thm
  import: select-thm
  import: cond-thm
  import: skolem
  package: hol-light-class-bool-2009.8.24
}

theory {
  import eta-thm;
  import select-thm;
  import cond-thm;
  import skolem;
  import bool;
}
