name: list
version: 1.54
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
  package: list-def-1.41
}

thm {
  import: def
  package: list-thm-1.34
}

dest {
  import: def
  import: thm
  package: list-dest-1.30
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.32
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.34
}

append {
  import: def
  import: dest
  import: length
  import: set
  package: list-append-1.34
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.34
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.34
}

last {
  import: def
  import: dest
  package: list-last-1.34
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.30
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.7
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
  package: list-nth-1.38
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.35
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  package: list-take-drop-1.36
}

interval {
  import: length
  import: nth
  package: list-interval-1.37
}

zip {
  import: def
  import: dest
  import: length
  package: list-zip-1.1
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.37
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
