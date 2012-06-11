name: list
version: 1.60
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
  package: list-def-1.44
}

thm {
  import: def
  package: list-thm-1.38
}

dest {
  import: def
  import: thm
  package: list-dest-1.33
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.35
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.37
}

append {
  import: def
  import: dest
  import: length
  import: set
  package: list-append-1.36
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.36
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.36
}

last {
  import: def
  import: dest
  package: list-last-1.36
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.33
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.10
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
  package: list-nth-1.42
}

replicate {
  import: length
  import: set
  import: nth
  package: list-replicate-1.39
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  package: list-take-drop-1.40
}

interval {
  import: length
  import: nth
  package: list-interval-1.41
}

zip {
  import: def
  import: dest
  import: length
  import: nth
  package: list-zip-1.6
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.40
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
