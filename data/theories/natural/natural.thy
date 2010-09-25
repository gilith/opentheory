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

# numeral-def {
#   import: num-def
#   package: numeral-def-1.0
# }

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
#   import: numeral-def
#   import: pre-def
#   import: add-def
}
