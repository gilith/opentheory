name: map-reduce-bit3x3
version: 1.0
description: Correctness proof for the map reduce 3x3 bit matrix example
author: Joe Hurd <joe@gilith.com>
license: MIT
#########################################################################
#
# Package names listed as requires like this:
#
requires: base
requires: list-fold
#
# use the theorems proved by theory packages to satisfy the assumptions
# of this theory. Do you need to add any package names here?
#########################################################################
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"

#########################################################################
#
# The body of the theory file is a list of theory blocks like this:
#
sat {
  package: map-reduce-bit3x3-sat-1.1
}

product {
  import: sat
  package: map-reduce-bit3x3-product-1.2
}

main {
  import: product
}
#
# that define the wiring diagram of the theory. See the theory file
# list-fold/list-fold.thy for an example of a theory wiring subtheory
# packages together.
#
# The desired end-result is a theory with no unsatisfied assumptions
# and the correctness theorem for parallelized matrix product
#
#   |- !l1 l2.
#        Bit3x3.product (l1 @ l2) =
#        Bit3x3.* (Bit3x3.product l1) (Bit3x3.product l2)
#
# among its list of theorems.
#
# Use the command
#
#   opentheory info map-reduce-bit3x3.thy
#
# at any time to see the unsatisfied assumptions and theorems proved by
# this theory.
#########################################################################
