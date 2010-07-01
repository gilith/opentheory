name: hol-light-arith
version: 2009.8.24
description: HOL Light arith theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

arith-pre-def {
  package: hol-light-arith-pre-def-2009.8.24
}

arith-add-def {
  import: arith-pre-def
  package: hol-light-arith-add-def-2009.8.24
}

arith-add-thm {
  import: arith-pre-def
  import: arith-add-def
  package: hol-light-arith-add-thm-2009.8.24
}

arith-numeral-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  package: hol-light-arith-numeral-thm-2009.8.24
}

arith-mult-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  package: hol-light-arith-mult-def-2009.8.24
}

arith-mult-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  package: hol-light-arith-mult-thm-2009.8.24
}

arith-exp-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  package: hol-light-arith-exp-def-2009.8.24
}

arith-exp-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  package: hol-light-arith-exp-thm-2009.8.24
}

arith-ord-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  package: hol-light-arith-ord-def-2009.8.24
}

arith-ord-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  package: hol-light-arith-ord-thm-2009.8.24
}

arith-even-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  package: hol-light-arith-even-def-2009.8.24
}

arith-even-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  package: hol-light-arith-even-thm-2009.8.24
}

arith-sub-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  package: hol-light-arith-sub-def-2009.8.24
}

arith-sub-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  package: hol-light-arith-sub-thm-2009.8.24
}

arith-fact-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  package: hol-light-arith-fact-def-2009.8.24
}

arith-fact-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  package: hol-light-arith-fact-thm-2009.8.24
}

arith-exp-thm-more {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  package: hol-light-arith-exp-thm-more-2009.8.24
}

arith-div-exist {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  package: hol-light-arith-div-exist-2009.8.24
}

arith-div-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  package: hol-light-arith-div-def-2009.8.24
}

arith-div-thm {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-def
  package: hol-light-arith-div-thm-2009.8.24
}

arith-max-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-def
  import: arith-div-thm
  package: hol-light-arith-max-def-2009.8.24
}

arith-minimal-def {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-def
  import: arith-div-thm
  import: arith-max-def
  package: hol-light-arith-minimal-def-2009.8.24
}

arith-minimal-alt {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-def
  import: arith-div-thm
  import: arith-max-def
  import: arith-minimal-def
  package: hol-light-arith-minimal-alt-2009.8.24
}

main {
  import: arith-pre-def
  import: arith-add-def
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-def
  import: arith-mult-thm
  import: arith-exp-def
  import: arith-exp-thm
  import: arith-ord-def
  import: arith-ord-thm
  import: arith-even-def
  import: arith-even-thm
  import: arith-sub-def
  import: arith-sub-thm
  import: arith-fact-def
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-def
  import: arith-div-thm
  import: arith-max-def
  import: arith-minimal-alt
}
