name: hol-light-arith-thm
version: 2009.8.24
description: HOL Light arith theory theorems
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require add-thm {
  package: hol-light-arith-add-thm-2009.8.24
}

require numeral-thm {
  import: add-thm
  package: hol-light-arith-numeral-thm-2009.8.24
}

require mult-thm {
  import: add-thm
  import: numeral-thm
  package: hol-light-arith-mult-thm-2009.8.24
}

require exp-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  package: hol-light-arith-exp-thm-2009.8.24
}

require ord-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  package: hol-light-arith-ord-thm-2009.8.24
}

require even-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  package: hol-light-arith-even-thm-2009.8.24
}

require sub-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  import: even-thm
  package: hol-light-arith-sub-thm-2009.8.24
}

require fact-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  import: even-thm
  import: sub-thm
  package: hol-light-arith-fact-thm-2009.8.24
}

require exp-thm-more {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  import: even-thm
  import: sub-thm
  import: fact-thm
  package: hol-light-arith-exp-thm-more-2009.8.24
}

require div-exist {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  import: even-thm
  import: sub-thm
  import: fact-thm
  import: exp-thm-more
  package: hol-light-arith-div-exist-2009.8.24
}

require div-thm {
  import: add-thm
  import: numeral-thm
  import: mult-thm
  import: exp-thm
  import: ord-thm
  import: even-thm
  import: sub-thm
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  package: hol-light-arith-div-thm-2009.8.24
}

theory {
  import add-thm;
  import numeral-thm;
  import mult-thm;
  import exp-thm;
  import ord-thm;
  import even-thm;
  import sub-thm;
  import fact-thm;
  import exp-thm-more;
  import div-exist;
  import div-thm;
}
