name: function
version: 1.0
description: Basic function theory
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Data.Function"

function-def {
  package: function-def-1.0
}

function-comb {
  import: function-def
  package: function-comb-1.0
}

main {
  import: function-def
  import: function-comb
}
