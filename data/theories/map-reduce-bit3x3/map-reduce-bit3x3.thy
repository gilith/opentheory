name: map-reduce-bit3x3
version: 1.2
description: Correctness proof for the map reduce 3x3 bit matrix example
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"

sat {
  package: map-reduce-bit3x3-sat-1.2
}

product {
  import: sat
  package: map-reduce-bit3x3-product-1.4
}

main {
  import: product
}
