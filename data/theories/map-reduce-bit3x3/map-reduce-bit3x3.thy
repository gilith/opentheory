name: map-reduce-bit3x3
version: 1.3
description: Correctness proof for the map reduce 3x3 bit matrix example
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"

sat {
  package: map-reduce-bit3x3-sat-1.3
}

product {
  import: sat
  package: map-reduce-bit3x3-product-1.5
}

main {
  import: product
}
