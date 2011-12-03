name: list
version: 1.42
description: List types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-def-1.31
}

thm {
  import: def
  package: list-thm-1.28
}

dest {
  import: def
  import: thm
  package: list-dest-1.24
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.25
}

set {
  import: def
  import: thm
  import: length
  package: list-set-1.26
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.25
}

map {
  import: def
  import: thm
  import: length
  import: set
  import: append
  package: list-map-1.26
}

quant {
  import: def
  import: set
  import: append
  import: map
  package: list-quant-1.25
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.26
}

last {
  import: def
  import: thm
  import: append
  import: set
  package: list-last-1.27
}

reverse {
  import: def
  import: length
  import: set
  import: append
  package: list-reverse-1.24
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
  package: list-nth-1.28
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.27
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
  package: list-member-1.30
}

concat {
  import: def
  import: dest
  import: append
  import: quant
  package: list-concat-1.25
}

take-drop {
  import: def
  import: thm
  import: dest
  import: append
  import: length
  import: nth
  package: list-take-drop-1.28
}

interval {
  import: length
  import: nth
  package: list-interval-1.28
}

zipwith {
  import: def
  import: dest
  import: length
  package: list-zipwith-1.26
}

nub {
  import: def
  import: reverse
  import: length
  import: member
  package: list-nub-1.30
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
