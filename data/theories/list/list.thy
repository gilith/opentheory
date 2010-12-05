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

filter {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  package: list-filter-1.0
}

last {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  package: list-last-1.0
}

reverse {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  package: list-reverse-1.0
}

length {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  package: list-length-1.0
}

nth {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  import: length
  package: list-nth-1.0
}

duplicate {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  import: length
  import: nth
  import: duplicate
  package: list-duplicate-1.0
}

member {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  import: length
  import: nth
  import: duplicate
  package: list-member-1.0
}

main {
  import: def
  import: thm
  import: dest
  import: append
  import: map
  import: quant
  import: filter
  import: last
  import: reverse
  import: length
  import: nth
  import: duplicate
  import: member
}
