name: natural-factorial
version: 1.38
description: Natural number factorial
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-factorial-def-1.35
  checksum: 2f15571a77606a3d04312a61e5e98a87be281316
}

thm {
  import: def
  package: natural-factorial-thm-1.34
  checksum: 5ba152a10f9988614252a675ec707a6b4fecc63c
}

main {
  import: def
  import: thm
}
