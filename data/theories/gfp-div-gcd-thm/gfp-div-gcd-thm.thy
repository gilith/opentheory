name: gfp-div-gcd-thm
version: 1.55
description: Correctness of a GF(p) division algorithm based on gcd
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-10-13
requires: bool
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-gcd-def
requires: gfp-div-thm
requires: gfp-thm
requires: gfp-witness
requires: natural
requires: natural-gcd
requires: natural-prime
show: "Data.Bool"
show: "Number.GF(p)"
show: "Number.Natural"

main {
  article: "gfp-div-gcd-thm.art"
}
