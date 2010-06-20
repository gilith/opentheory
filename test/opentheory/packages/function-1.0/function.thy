name: function
version: 1.0
description: Basic function theory
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Function"

function-def {
  package: function-def-1.0
}

function-thm {
  import: function-def
  package: function-thm-1.0
}

main {
  import: function-def
  import: function-thm
}
