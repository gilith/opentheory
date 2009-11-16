name: hol-light-class
version: 2009.8.24
description: HOL Light class theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require eta {
  package: hol-light-class-eta-2009.8.24
}

require select {
  require: eta
  package: hol-light-class-select-2009.8.24
}

require cond {
  require: eta
  require: select
  package: hol-light-class-cond-2009.8.24
}

require skolem {
  require: eta
  require: select
  require: cond
  package: hol-light-class-skolem-2009.8.24
}

require bool {
  require: eta
  require: select
  require: cond
  require: skolem
  package: hol-light-class-bool-2009.8.24
}

theory {
  import eta;
  import select;
  import cond;
  import skolem;
  import bool;
}
