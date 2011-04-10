name: modular
version: 1.2
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Numeral"

mod {
  package: modular-mod-1.0
}

equiv {
  import: mod
  package: modular-equiv-1.1
}

def {
  import: mod
  import: equiv
  package: modular-def-1.1
}

thm {
  import: mod
  import: def
  package: modular-thm-1.1
}

main {
  import: mod
  import: def
  import: thm
}
