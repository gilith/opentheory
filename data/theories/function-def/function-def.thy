name: function-def
version: 1.3
description: Definition of function constants
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

function-def-comb {
  package: function-def-comb-1.0
}

function-def-inj-surj {
  package: function-def-inj-surj-1.0
}

main {
  import: function-def-comb
  import: function-def-inj-surj
}
