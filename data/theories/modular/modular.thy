name: modular
version: 1.21
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
  package: modular-equiv-1.16
}

def {
  import: mod
  import: equiv
  package: modular-def-1.19
}

thm {
  import: mod
  import: def
  package: modular-thm-1.10
}

main {
  import: mod
  import: def
  import: thm
}
