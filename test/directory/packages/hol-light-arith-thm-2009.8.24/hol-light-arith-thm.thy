name: hol-light-arith-thm
version: 2009.8.24
description: HOL Light arith theory theorems
author: Joe Hurd <joe@gilith.com>
license: HOLLight

arith-add-thm {
  package: hol-light-arith-add-thm-2009.8.24
}

arith-numeral-thm {
  import: arith-add-thm
  package: hol-light-arith-numeral-thm-2009.8.24
}

arith-mult-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  package: hol-light-arith-mult-thm-2009.8.24
}

arith-exp-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  package: hol-light-arith-exp-thm-2009.8.24
}

arith-ord-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  package: hol-light-arith-ord-thm-2009.8.24
}

arith-even-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  package: hol-light-arith-even-thm-2009.8.24
}

arith-sub-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  package: hol-light-arith-sub-thm-2009.8.24
}

arith-fact-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  import: arith-sub-thm
  package: hol-light-arith-fact-thm-2009.8.24
}

arith-exp-thm-more {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  import: arith-sub-thm
  import: arith-fact-thm
  package: hol-light-arith-exp-thm-more-2009.8.24
}

arith-div-exist {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  import: arith-sub-thm
  import: arith-fact-thm
  import: arith-exp-thm-more
  package: hol-light-arith-div-exist-2009.8.24
}

arith-div-thm {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  import: arith-sub-thm
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  package: hol-light-arith-div-thm-2009.8.24
}

main {
  import: arith-add-thm
  import: arith-numeral-thm
  import: arith-mult-thm
  import: arith-exp-thm
  import: arith-ord-thm
  import: arith-even-thm
  import: arith-sub-thm
  import: arith-fact-thm
  import: arith-exp-thm-more
  import: arith-div-exist
  import: arith-div-thm
}
