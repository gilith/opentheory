name: list
version: 1.67
description: List types
author: Joe Hurd <joe@gilith.com>
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
  package: list-def-1.51
}

thm {
  import: def
  package: list-thm-1.45
}

dest {
  import: def
  import: thm
  package: list-dest-1.38
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.40
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.42
}

append {
  import: def
  import: dest
  import: length
  import: set
  package: list-append-1.41
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.41
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.41
}

last {
  import: def
  import: dest
  package: list-last-1.41
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.38
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.15
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
  package: list-nth-1.48
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.45
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  package: list-take-drop-1.45
}

interval {
  import: length
  import: map
  import: nth
  package: list-interval-1.48
}

zip {
  import: def
  import: dest
  import: length
  import: nth
  package: list-zip-1.12
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.45
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
