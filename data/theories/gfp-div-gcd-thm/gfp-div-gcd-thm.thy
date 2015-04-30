name: gfp-div-gcd-thm
version: 1.64
description: Correctness of a GF(p) division algorithm based on gcd
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-17
requires: base
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-gcd-def
requires: gfp-div-thm
requires: gfp-thm
requires: gfp-witness
requires: natural-gcd
requires: natural-prime
show: "Data.Bool"
show: "Number.GF(p)"
show: "Number.Natural"

main {
  article: "gfp-div-gcd-thm.art"
}
