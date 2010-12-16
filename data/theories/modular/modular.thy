name: modular
version: 1.0
description: Functor theory of modular arithmetic
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Number.Modular"
show: "Number.Numeral"

mod {
  package: modular-mod-1.0
}

equiv {
  import: mod
  package: modular-equiv-1.0
}

def {
  import: mod
  import: equiv
  package: modular-def-1.0
}

main {
  import: def
}
