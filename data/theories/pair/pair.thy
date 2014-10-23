name: pair
version: 1.25
description: Product types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Pair"

def {
  package: pair-def-1.22
  checksum: 1158034ec61f4e7d04fc234c27f8eb0965d33710
}

thm {
  import: def
  package: pair-thm-1.26
  checksum: 0fb1f10dda9d57a53eceac2196ec9397f3dd3d39
}

main {
  import: def
  import: thm
}
