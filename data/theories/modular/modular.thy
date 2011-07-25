name: modular
version: 1.11
description: Parametric theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Natural" as "Natural"

mod {
  package: modular-mod-1.2
}

equiv {
  import: mod
  package: modular-equiv-1.9
}

def {
  import: mod
  import: equiv
  package: modular-def-1.12
}

thm {
  import: mod
  import: def
  package: modular-thm-1.2
}

main {
  import: mod
  import: def
  import: thm
}
