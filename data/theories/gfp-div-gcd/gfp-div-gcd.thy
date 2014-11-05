name: gfp-div-gcd
version: 1.63
description: A GF(p) division algorithm based on gcd
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-thm
requires: gfp-thm
requires: gfp-witness
requires: natural
requires: natural-gcd
requires: natural-prime
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.Pair"
show: "Number.GF(p)"
show: "Number.Natural"

def {
  package: gfp-div-gcd-def-1.59
  checksum: f50ab28705a56bd10993881082605755ab6e445a
}

thm {
  import: def
  package: gfp-div-gcd-thm-1.62
  checksum: 28492f78178668df21335fbc042bca3b457f86e5
}

main {
  import: def
  import: thm
}
