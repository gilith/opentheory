name: list
version: 1.16
description: Basic theory of list types
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Function"
show: "Number.Natural"

def {
  package: list-def-1.10
}

thm {
  import: def
  package: list-thm-1.10
}

dest {
  import: def
  import: thm
  package: list-dest-1.12
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.11
}

set {
  import: def
  import: thm
  import: length
  package: list-set-1.10
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.11
}

map {
  import: def
  import: thm
  import: length
  import: set
  import: append
  package: list-map-1.11
}

quant {
  import: def
  import: append
  import: map
  package: list-quant-1.9
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.11
}

last {
  import: def
  import: thm
  import: append
  import: set
  package: list-last-1.10
}

reverse {
  import: def
  import: length
  import: set
  import: append
  package: list-reverse-1.11
}

nth {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  import: map
  import: last
  import: length
  package: list-nth-1.11
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.11
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
  package: list-member-1.13
}

concat {
  import: def
  import: dest
  import: append
  import: quant
  package: list-concat-1.11
}

take-drop {
  import: def
  import: thm
  import: dest
  import: append
  import: length
  import: nth
  package: list-take-drop-1.12
}

interval {
  import: length
  import: nth
  package: list-interval-1.12
}

zipwith {
  import: def
  import: dest
  import: length
  package: list-zipwith-1.12
}

nub {
  import: def
  import: reverse
  import: length
  import: member
  package: list-nub-1.13
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
