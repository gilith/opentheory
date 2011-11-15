name: modular
version: 1.20
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural" as "Natural"

mod {
  package: modular-mod-1.7
}

equiv {
  import: mod
  package: modular-equiv-1.15
}

def {
  import: mod
  import: equiv
  package: modular-def-1.18
}

thm {
  import: mod
  import: def
  package: modular-thm-1.9
}

main {
  import: mod
  import: def
  import: thm
}
