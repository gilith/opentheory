name: function
version: 1.51
description: Function operators and combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.18
  checksum: f2c922f5bb63ab2ac8032e9f18df879dc3a17124
}

thm {
  import: def
  package: function-thm-1.45
  checksum: d082753e3a15d801fe25feebedf13b9e52608cd4
}

main {
  import: def
  import: thm
}
