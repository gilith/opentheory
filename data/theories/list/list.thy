name: list
version: 1.92
description: List types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: list-def-1.58
}

thm {
  import: def
  package: list-thm-1.50
}

dest {
  import: def
  import: thm
  package: list-dest-1.44
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.45
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.48
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.52
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.48
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.48
}

last {
  import: def
  import: dest
  package: list-last-1.47
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.43
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.22
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
  package: list-nth-1.55
}

replicate {
  import: def
  import: thm
  import: length
  import: set
  import: append
  import: map
  import: nth
  package: list-replicate-1.56
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  import: replicate
  package: list-take-drop-1.56
}

interval {
  import: length
  import: map
  import: nth
  package: list-interval-1.55
}

zip {
  import: def
  import: dest
  import: length
  import: append
  import: nth
  package: list-zip-1.21
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.51
}

main {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  import: map
  import: filter
  import: last
  import: reverse
  import: fold
  import: nth
  import: replicate
  import: take-drop
  import: interval
  import: zip
  import: nub
}
