name: base
version: 1.0
description: Basic theories
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Function"
show: "Data.Pair"
show: "Data.Unit"
show: "Number.Natural"
show: "Number.Numeral"

require bool {
  package: bool-1.0
}

require unit {
  import: bool
  package: unit-1.0
}

require pair {
  import: bool
  package: pair-1.0
}

require function {
  import: bool
  package: function-1.0
}

require axiom-infinity {
  import: bool
  import: function
  package: axiom-infinity-1.0
}

require num {
  import: bool
  import: function
  import: axiom-infinity
  package: num-1.0
}

theory {
  import bool;
  import unit;
  import pair;
  import function;
  import axiom-infinity;
  import num;
}
