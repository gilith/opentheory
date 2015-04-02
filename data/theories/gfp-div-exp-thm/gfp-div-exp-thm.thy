name: gfp-div-exp-thm
version: 1.43
description: Correctness of a GF(p) exponentiation algorithm based on division
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-11-01
requires: base
requires: gfp-def
requires: gfp-div-def
requires: gfp-div-exp-def
requires: gfp-div-thm
requires: gfp-thm
requires: natural-fibonacci
show: "Data.Bool"
show: "Data.List"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

main {
  article: "gfp-div-exp-thm.art"
}
