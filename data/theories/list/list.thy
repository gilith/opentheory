name: list
version: 1.95
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
  package: list-def-1.60
  checksum: 54dc33bd918fa2997ab8afada7965e3a7467c81c
}

thm {
  import: def
  package: list-thm-1.52
  checksum: 2c03a2bd40b7ed62cbd9da55ac8f96238dd164e8
}

dest {
  import: def
  import: thm
  package: list-dest-1.46
  checksum: 1da4f4bf9c6666f00b972abe1b083c4d4d060603
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.48
  checksum: 17f637299ed8cea29b54fa4dbf99fc8122b6b343
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.51
  checksum: dace57c307b7613bce8edc198b89b90824b80c5b
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.55
  checksum: 9f6bbf5c4a0fd9993eac38cc9458c0997e5ffe6a
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.51
  checksum: c8082106d4519ad96e1090a028edb6e591b0396d
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.51
  checksum: 1452b8c6e124dd017866bff1dbfce57fd9e0a66d
}

last {
  import: def
  import: dest
  package: list-last-1.49
  checksum: bcb25034b0e6662645e3504090c3f66cb06e042c
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.46
  checksum: 36791d115c569cb282912e8aa6c3d357639d1bd9
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.25
  checksum: d76c942cfcf73d2ab3db5f79c65f68ef07541e4e
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
  package: list-nth-1.58
  checksum: 5c6a400eff4bb344b49bc4e35e1fce6113bb8eec
}

replicate {
  import: def
  import: thm
  import: length
  import: set
  import: append
  import: map
  import: nth
  package: list-replicate-1.59
  checksum: f844e7ce6759d1360d56ba6562473dc77897603e
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  import: replicate
  package: list-take-drop-1.59
  checksum: 4fd78981e767dea9a5fb15d9cec21a6cfcce8a74
}

interval {
  import: length
  import: map
  import: nth
  package: list-interval-1.58
  checksum: ab627be8cf87f3837691a13e9f5fe4a563507a2a
}

zip {
  import: def
  import: dest
  import: length
  import: append
  import: nth
  package: list-zip-1.24
  checksum: 12e6525796fa8663b603b04f166024c8c851d581
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.54
  checksum: 9369c0f364d133bee692cb667a082da5887a612e
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
