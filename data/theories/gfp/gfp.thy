name: gfp
version: 1.9
description: Finite fields GF(p)
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Number.GF(p)"
show: "Number.Natural" as "Natural"

def {
  package: gfp-def-1.2
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Number.GF(p).gfp"
  interpret: const "Number.Modular.*" as "Number.GF(p).*"
  interpret: const "Number.Modular.+" as "Number.GF(p).+"
  interpret: const "Number.Modular.-" as "Number.GF(p).-"
  interpret: const "Number.Modular.<" as "Number.GF(p).<"
  interpret: const "Number.Modular.<=" as "Number.GF(p).<="
  interpret: const "Number.Modular.~" as "Number.GF(p).~"
  interpret: const "Number.Modular.fromNatural" as "Number.GF(p).fromNatural"
  interpret: const "Number.Modular.modulus" as "Number.GF(p).oddprime"
  interpret: const "Number.Modular.toNatural" as "Number.GF(p).toNatural"
  package: modular-1.23
}

thm {
  import: def
  import: modular
  package: gfp-thm-1.5
}

inverse {
  import: def
  import: modular
  import: thm
  package: gfp-inverse-1.8
}

main {
  import: def
  import: modular
  import: thm
  import: inverse
}
