name: map-reduce-bit3x3
version: 1.5
description: The map reduce 3x3 bit matrix example
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
hol-light-int-file: hol-light.int

sat {
  package: map-reduce-bit3x3-sat-1.4
}

product {
  import: sat
  package: map-reduce-bit3x3-product-1.6
}

main {
  import: product
}
