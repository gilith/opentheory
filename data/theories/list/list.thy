name: list
version: 1.97
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
  package: list-def-1.62
  checksum: 7705f76224bf958085e6f50b25444a401386631d
}

thm {
  import: def
  package: list-thm-1.54
  checksum: 6b679e8c60feaa38c7c85887716b299bd5888234
}

dest {
  import: def
  import: thm
  package: list-dest-1.48
  checksum: 24dcaa169f5cb1781e03cade96641ea2490243ac
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.50
  checksum: 25baeab9406a9e7a1482a9f9c68be176a48253c8
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.53
  checksum: b14faf1cd37a22a862fac056cfc76d31a935f37c
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.57
  checksum: c804ed730ec9eae19e37f35a0eb7d246cd9851d4
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.53
  checksum: c1e62333caf77fca43f93acc9192408444152fc0
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.53
  checksum: e5df8b3000b5e1ac7582c2ae2cf9f0b0115358ce
}

last {
  import: def
  import: dest
  package: list-last-1.51
  checksum: 7f8c2eae211ac676c0210f811eb30366b8995c99
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.48
  checksum: 42c6c6b3cb38d1882711cc89b3e7394b9b67836f
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.27
  checksum: 5d205c895bc58793254c588cd3ce4dcb3688355e
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
  package: list-nth-1.60
  checksum: c7c9a4f8b9123cb1cb8d3f262a30d40e08b2913a
}

replicate {
  import: def
  import: thm
  import: length
  import: set
  import: append
  import: map
  import: nth
  package: list-replicate-1.61
  checksum: ae3be78e5f6669d68b8d8756f15569b1e1e0489c
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  import: replicate
  package: list-take-drop-1.61
  checksum: 9dac544a2c4a45be708fd64506a9749ab63b2530
}

interval {
  import: length
  import: map
  import: nth
  package: list-interval-1.60
  checksum: fc070996542cb994277a58adad645ec20770537c
}

zip {
  import: def
  import: dest
  import: length
  import: append
  import: nth
  package: list-zip-1.26
  checksum: dc0c419fefb26b237fb661ff1702f08663e2c819
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.56
  checksum: 58cd25730ee1d69aebd72619b91e100f00cdf56d
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
