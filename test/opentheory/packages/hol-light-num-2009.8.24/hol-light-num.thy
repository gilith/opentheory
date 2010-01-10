name: hol-light-num
version: 2009.8.24
description: HOL Light num theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require inj-surj {
  package: hol-light-num-inj-surj-2009.8.24
}

require axiom {
  import: inj-surj
  package: hol-light-num-infinity-2009.8.24
}

require def {
  import: inj-surj
  import: axiom
  package: hol-light-num-def-2009.8.24
}

require alt {
  import: inj-surj
  import: axiom
  import: def
  package: hol-light-num-alt-2009.8.24
}

require numeral {
  import: inj-surj
  import: axiom
  import: alt
  package: hol-light-num-numeral-2009.8.24
}

require thm {
  import: inj-surj
  import: axiom
  import: alt
  import: numeral
  package: hol-light-num-thm-2009.8.24
}

require bit {
  import: inj-surj
  import: axiom
  import: alt
  import: numeral
  import: thm
  package: hol-light-num-bit-2009.8.24
}

theory {
  import inj-surj;
  import axiom;
  import alt;
  import numeral;
  import thm;
  import bit;
}
