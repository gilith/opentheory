name: list
version: 1.0
description: Basic theory of list types
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-def-1.0
}

thm {
  import: def
  package: list-thm-1.0
}

dest {
  import: def
  import: thm
  package: list-dest-1.0
}

append {
  import: def
  import: thm
  import: dest
  package: list-append-1.0
}

map {
  import: def
  import: thm
  import: dest
  import: append
  package: list-map-1.0
}

quant {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  package: list-quant-1.0
}

reverse {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  package: list-reverse-1.0
}

main {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: reverse
}
