name: list-last
version: 1.49
description: The last list function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
show: "Data.Bool"
show: "Data.List"

def {
  package: list-last-def-1.44
  checksum: 73fde5af30eac42a4011585ee19dc63a1ff53fc8
}

thm {
  import: def
  package: list-last-thm-1.38
  checksum: dac5e19b77675e226bc4ae0e1fd892af51a7bde1
}

main {
  import: def
  import: thm
}
