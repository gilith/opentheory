name: list
version: 1.98
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
  package: list-def-1.63
  checksum: 14ec565fcc461c3e04db57f3fca0e89ef3e293a4
}

thm {
  import: def
  package: list-thm-1.54
  checksum: 6b679e8c60feaa38c7c85887716b299bd5888234
}

dest {
  import: def
  import: thm
  package: list-dest-1.49
  checksum: 2cc5c045741b3cefd3de25229ea3742ff12f626c
}

length {
  import: def
  import: thm
  import: dest
  package: list-length-1.51
  checksum: ef787afb32144114a90977765d0a13def40289fd
}

set {
  import: def
  import: dest
  import: length
  package: list-set-1.54
  checksum: 20b2942b2b9641365f92d0dabee146ec1f4be9f7
}

append {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  package: list-append-1.58
  checksum: cb776d9276ea7a4f791a98db414fedc55adbde00
}

map {
  import: def
  import: thm
  import: dest
  import: length
  import: set
  import: append
  package: list-map-1.54
  checksum: fc460c62d5a52430cc8c5402798eb6b19ef812be
}

filter {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-filter-1.54
  checksum: b94b28a3f965671cb7077d216175c99ccbc584af
}

last {
  import: def
  import: dest
  package: list-last-1.52
  checksum: d2ebfe4d8bbfe83741e0e26162e8345222b0e453
}

reverse {
  import: def
  import: length
  import: set
  import: append
  import: map
  package: list-reverse-1.49
  checksum: ff8986ccfed902d3312c0fcd953b9a1c20187f16
}

fold {
  import: def
  import: length
  import: append
  import: reverse
  package: list-fold-1.28
  checksum: bfc16b9693e0e3c35a7806737e79874fda083251
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
  package: list-nth-1.61
  checksum: 35fdd28af7d3bc96c4c4f9ec6b04a46780c3e3a3
}

replicate {
  import: def
  import: thm
  import: length
  import: set
  import: append
  import: map
  import: nth
  package: list-replicate-1.62
  checksum: e35dfead4d202a6a4ae87c2a26154e1a6e4f6c7f
}

take-drop {
  import: def
  import: thm
  import: dest
  import: length
  import: append
  import: nth
  import: replicate
  package: list-take-drop-1.62
  checksum: 5207642d41b4d0434e0245c8953838ce801aa38c
}

interval {
  import: length
  import: map
  import: nth
  package: list-interval-1.61
  checksum: be1fcf160a98813a1b44ff655af63c202df01feb
}

zip {
  import: def
  import: dest
  import: length
  import: append
  import: nth
  package: list-zip-1.27
  checksum: 9dd80cccceb4aa7681adb4536924c7ffa52378fe
}

nub {
  import: def
  import: length
  import: reverse
  import: set
  package: list-nub-1.57
  checksum: d5f1a3e97bade3b0aa1b96c3b6ed2be53243d147
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
