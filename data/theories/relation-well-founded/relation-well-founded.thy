name: relation-well-founded
version: 1.55
description: Well-founded relations
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: relation-def
requires: relation-thm
show: "Data.Bool"
show: "Data.Pair"
show: "Relation"

def {
  package: relation-well-founded-def-1.38
  checksum: 8dac1adb31a0915d7cf32d96dd58e8b7505c72cd
}

thm {
  import: def
  package: relation-well-founded-thm-1.62
  checksum: 4550bb60018612445e0962004b7a5756377dc911
}

main {
  import: def
  import: thm
}
