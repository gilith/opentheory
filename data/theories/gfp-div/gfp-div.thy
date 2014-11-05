name: gfp-div
version: 1.74
description: GF(p) field division
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-def
requires: gfp-thm
requires: gfp-witness
requires: list
requires: natural
requires: natural-divides
requires: natural-fibonacci
requires: natural-gcd
requires: natural-prime
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

def {
  package: gfp-div-def-1.65
  checksum: 6ff4182a079817316b5f4545b0cdaee3dd4980f1
}

thm {
  import: def
  package: gfp-div-thm-1.63
  checksum: 3ea12b0906a480861f2cb2680451394b132c99ef
}

gcd {
  import: def
  import: thm
  package: gfp-div-gcd-1.63
  checksum: 22d366f7f4cfbc85855461b51328e298b08284db
}

exp {
  import: def
  import: thm
  package: gfp-div-exp-1.42
  checksum: d6f34753a3ebc4c064dc51707afea160f10d2de9
}

main {
  import: def
  import: thm
  import: gcd
  import: exp
}
