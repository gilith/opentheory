name: list
version: 1.24
description: Standard theory of lists
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"

def {
  package: list-def-1.18
}

thm {
  import: def
  package: list-thm-1.17
}

dest {
  import: def
  import: thm
  package: list-dest-1.14
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.14
}

set {
  import: def
  import: thm
  import: length
  package: list-set-1.14
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.14
}

map {
  import: def
  import: thm
  import: length
  import: set
  import: append
  package: list-map-1.14
}

quant {
  import: def
  import: set
  import: append
  import: map
  package: list-quant-1.12
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.14
}

last {
  import: def
  import: thm
  import: append
  import: set
  package: list-last-1.14
}

reverse {
  import: def
  import: length
  import: set
  import: append
  package: list-reverse-1.14
}

nth {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  import: map
  import: quant
  import: last
  import: length
  package: list-nth-1.17
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.16
}

member {
  import: def
  import: set
  import: append
  import: map
  import: quant
  import: filter
  import: reverse
  import: length
  import: nth
  package: list-member-1.18
}

concat {
  import: def
  import: dest
  import: append
  import: quant
  package: list-concat-1.13
}

take-drop {
  import: def
  import: thm
  import: dest
  import: append
  import: length
  import: nth
  package: list-take-drop-1.17
}

interval {
  import: length
  import: nth
  package: list-interval-1.17
}

zipwith {
  import: def
  import: dest
  import: length
  package: list-zipwith-1.15
}

nub {
  import: def
  import: reverse
  import: length
  import: member
  package: list-nub-1.17
}

main {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  import: nth
  import: replicate
  import: member
  import: concat
  import: take-drop
  import: interval
  import: zipwith
  import: nub
}
