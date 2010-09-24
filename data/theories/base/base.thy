name: base
version: 1.0
description: Basic theories
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Data.Pair"
show: "Data.Unit"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"

bool {
  package: bool-1.0
}

unit {
  import: bool
  package: unit-1.0
}

function {
  import: bool
  package: function-1.0
}

pair {
  import: bool
  package: pair-1.0
}

# axiom-infinity {
#   import: bool
#   import: function
#   package: axiom-infinity-1.0
# }

# num {
#   import: bool
#   import: function
#   import: axiom-infinity
#   package: num-1.0
# }

main {
  import: bool
  import: unit
  import: function
  import: pair
#   import: axiom-infinity
#   import: num
}
