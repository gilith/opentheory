name: hol-light-num
version: 2009.8.24
description: HOL Light num theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

num-inj-surj {
  package: hol-light-num-inj-surj-2009.8.24
}

num-infinity {
  import: num-inj-surj
  package: hol-light-num-infinity-2009.8.24
}

num-def {
  import: num-inj-surj
  import: num-infinity
  package: hol-light-num-def-2009.8.24
}

num-alt {
  import: num-inj-surj
  import: num-infinity
  import: num-def
  package: hol-light-num-alt-2009.8.24
}

num-numeral {
  import: num-inj-surj
  import: num-infinity
  import: num-alt
  package: hol-light-num-numeral-2009.8.24
}

num-thm {
  import: num-inj-surj
  import: num-infinity
  import: num-alt
  import: num-numeral
  package: hol-light-num-thm-2009.8.24
}

num-bit {
  import: num-inj-surj
  import: num-infinity
  import: num-alt
  import: num-numeral
  import: num-thm
  package: hol-light-num-bit-2009.8.24
}

main {
  import: num-inj-surj
  import: num-infinity
  import: num-alt
  import: num-numeral
  import: num-thm
  import: num-bit
}
