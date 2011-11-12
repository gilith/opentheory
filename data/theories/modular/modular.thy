name: modular
version: 1.16
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural" as "Natural"

mod {
  package: modular-mod-1.6
}

equiv {
  import: mod
  package: modular-equiv-1.12
}

def {
  import: mod
  import: equiv
  package: modular-def-1.15
}

thm {
  import: mod
  import: def
  package: modular-thm-1.7
}

main {
  import: mod
  import: def
  import: thm
}
