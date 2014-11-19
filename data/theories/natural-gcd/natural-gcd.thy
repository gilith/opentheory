name: natural-gcd
version: 1.52
description: Natural number greatest common divisor
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-def-1.41
  checksum: 3f5fc2ffabaf8ac84d6100fb288641ccfb88e5a9
}

thm {
  import: def
  package: natural-gcd-thm-1.49
  checksum: f3b0197d33cccf61e56bf2828c44cbadbfd95be9
}

lcm {
  import: def
  import: thm
  package: natural-gcd-lcm-1.38
  checksum: 2ba23a73c971509db8480a85451178d68cd8051e
}

main {
  import: def
  import: thm
  import: lcm
}
