name: natural
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Function"
show: "Number.Natural"
show: "Number.Numeral"

natural-def {
  package: natural-def-1.0
}

natural-recursion {
  import: natural-def
  package: natural-recursion-1.0
}

natural-cases {
  import: natural-def
  import: natural-recursion
  package: natural-cases-1.0
}

natural-numeral {
  import: natural-def
  import: natural-recursion
  import: natural-cases
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
  import: natural-def
  import: natural-recursion
  import: natural-cases
  import: natural-numeral
#   import: pre-def
#   import: add-def
}
