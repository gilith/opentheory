name: pair
version: 1.0
description: Definition and theorems about the product type
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Pair"

pair-def {
  package: pair-def-1.0
}

pair-induct {
  import: pair-def
  package: pair-induct-1.0
}

pair-quant {
  import: pair-def
  import: pair-induct
  package: pair-quant-1.0
}

main {
  import: pair-def
  import: pair-induct
  import: pair-quant
}
