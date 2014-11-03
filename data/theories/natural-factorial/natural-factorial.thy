name: natural-factorial
version: 1.37
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
  package: natural-factorial-def-1.34
  checksum: cf01f47ac8a115cf856b3572a28912b45f8f7741
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
