name: gfp-div-exp-thm
version: 1.4
description: Properties of a GF(p) exponentiation algorithm based on division
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-03-18
requires: bool
requires: list
requires: natural
requires: natural-fibonacci
requires: gfp-def
requires: gfp-thm
requires: gfp-div-def
requires: gfp-div-thm
requires: gfp-div-exp-def
show: "Data.Bool"
show: "Data.List"
show: "Number.GF(p)"
show: "Number.Natural"
show: "Number.Natural.Fibonacci"

main {
  article: "gfp-div-exp-thm.art"
}
