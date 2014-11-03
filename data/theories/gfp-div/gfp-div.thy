name: gfp-div
version: 1.73
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
  package: gfp-div-def-1.64
  checksum: 3c132af477bf70ceabc8c0159ef9bd0dec4e46a6
}

thm {
  import: def
  package: gfp-div-thm-1.63
  checksum: 3ea12b0906a480861f2cb2680451394b132c99ef
}

gcd {
  import: def
  import: thm
  package: gfp-div-gcd-1.62
  checksum: ad7c54dec8156c33853c20f4a18bd88e7538ba41
}

exp {
  import: def
  import: thm
  package: gfp-div-exp-1.41
  checksum: d60a48458b260f21ed986c3cdc7d070239537a2d
}

main {
  import: def
  import: thm
  import: gcd
  import: exp
}
