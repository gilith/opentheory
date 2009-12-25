name: hol-light-class
version: 2009.8.24
description: HOL Light class theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require eta {
  package: hol-light-class-eta-2009.8.24
}

require select {
  import: eta
  package: hol-light-class-select-2009.8.24
}

require cond {
  import: eta
  import: select
  package: hol-light-class-cond-2009.8.24
}

require skolem {
  import: eta
  import: select
  import: cond
  package: hol-light-class-skolem-2009.8.24
}

require bool {
  import: eta
  import: select
  import: cond
  import: skolem
  package: hol-light-class-bool-2009.8.24
}

theory {
  import eta;
  import select;
  import cond;
  import skolem;
  import bool;
}
