name: natural
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Function"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-def-1.0
}

recursion {
  import: def
  package: natural-recursion-1.0
}

cases {
  import: def
  import: recursion
  package: natural-cases-1.0
}

numeral {
  import: def
  import: recursion
  import: cases
  package: natural-numeral-1.0
}

# pre-def {
#   import: num-def
#   import: numeral-def
#   package: num-pre-def-1.0
# }

# add-def {
#   import: num-def
#   import: numeral-def
#   package: num-add-def-1.0
# }

main {
  import: def
  import: recursion
  import: cases
  import: numeral
#   import: pre-def
#   import: add-def
}
