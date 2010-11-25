name: bool-int
version: 1.0
description: Intuitionistic boolean theorems
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"

bool-int-true {
  package: bool-int-true-1.0
}

bool-int-eq {
  import: bool-int-true
  package: bool-int-eq-1.0
}

bool-int-and {
  import: bool-int-true
  package: bool-int-and-1.0
}

bool-int-or {
  import: bool-int-true
  package: bool-int-or-1.0
}

bool-int-imp {
  import: bool-int-true
  package: bool-int-imp-1.0
}

bool-int-or-dist {
  import: bool-int-true
  package: bool-int-or-dist-1.0
}

bool-int-quant-triv {
  import: bool-int-true
  package: bool-int-quant-triv-1.0
}

bool-int-rewr {
  import: bool-int-true
  package: bool-int-rewr-1.0
}

bool-int-quant {
  import: bool-int-true
  import: bool-int-eq
  import: bool-int-and
  import: bool-int-rewr
  package: bool-int-quant-1.0
}

bool-int-mono {
  import: bool-int-true
  package: bool-int-mono-1.0
}

main {
  import: bool-int-true
  import: bool-int-eq
  import: bool-int-and
  import: bool-int-or
  import: bool-int-imp
  import: bool-int-or-dist
  import: bool-int-quant-triv
  import: bool-int-rewr
  import: bool-int-quant
  import: bool-int-mono
}
