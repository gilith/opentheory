name: pair
version: 1.0
description: Definition and theorems about the product type
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Data.Pair"

pair-def {
  package: pair-def-1.0
}

pair-induct {
  import: pair-def
  package: pair-induct-1.0
}

pair-abs {
  import: pair-induct
  package: pair-abs-1.0
}

main {
  import: pair-def
  import: pair-induct
  import: pair-abs
}
