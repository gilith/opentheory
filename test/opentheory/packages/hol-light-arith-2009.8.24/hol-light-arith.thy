name: hol-light-arith
version: 2009.8.24
description: HOL Light arith theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

require pre-def {
  package: hol-light-arith-pre-def-2009.8.24
}

require add-def {
  import: pre-def
  package: hol-light-arith-add-def-2009.8.24
}

require add-thm {
  import: pre-def
  import: add-def
  package: hol-light-arith-add-thm-2009.8.24
}

require numeral-thm {
  import: pre-def
  import: add-def
  import: add-thm
  package: hol-light-arith-numeral-thm-2009.8.24
}

require mult-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  package: hol-light-arith-mult-def-2009.8.24
}

require mult-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  package: hol-light-arith-mult-thm-2009.8.24
}

require exp-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  package: hol-light-arith-exp-def-2009.8.24
}

require exp-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  package: hol-light-arith-exp-thm-2009.8.24
}

require ord-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  package: hol-light-arith-ord-def-2009.8.24
}

require ord-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  package: hol-light-arith-ord-thm-2009.8.24
}

require even-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  package: hol-light-arith-even-def-2009.8.24
}

require even-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  package: hol-light-arith-even-thm-2009.8.24
}

require sub-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  package: hol-light-arith-sub-def-2009.8.24
}

require sub-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  package: hol-light-arith-sub-thm-2009.8.24
}

require fact-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  package: hol-light-arith-fact-def-2009.8.24
}

require fact-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  package: hol-light-arith-fact-thm-2009.8.24
}

require exp-thm-more {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  package: hol-light-arith-exp-thm-more-2009.8.24
}

require div-exist {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  package: hol-light-arith-div-exist-2009.8.24
}

require div-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  package: hol-light-arith-div-def-2009.8.24
}

require div-thm {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  import: div-def
  package: hol-light-arith-div-thm-2009.8.24
}

require max-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  import: div-def
  import: div-thm
  package: hol-light-arith-max-def-2009.8.24
}

require minimal-def {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  import: div-def
  import: div-thm
  import: max-def
  package: hol-light-arith-minimal-def-2009.8.24
}

require minimal-alt {
  import: pre-def
  import: add-def
  import: add-thm
  import: numeral-thm
  import: mult-def
  import: mult-thm
  import: exp-def
  import: exp-thm
  import: ord-def
  import: ord-thm
  import: even-def
  import: even-thm
  import: sub-def
  import: sub-thm
  import: fact-def
  import: fact-thm
  import: exp-thm-more
  import: div-exist
  import: div-def
  import: div-thm
  import: max-def
  import: minimal-def
  package: hol-light-arith-minimal-alt-2009.8.24
}

theory {
  import pre-def;
  import add-def;
  import add-thm;
  import numeral-thm;
  import mult-def;
  import mult-thm;
  import exp-def;
  import exp-thm;
  import ord-def;
  import ord-thm;
  import even-def;
  import even-thm;
  import sub-def;
  import sub-thm;
  import fact-def;
  import fact-thm;
  import exp-thm-more;
  import div-exist;
  import div-def;
  import div-thm;
  import max-def;
  import minimal-alt;
}
