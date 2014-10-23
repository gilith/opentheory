name: monoid-mult-add-thm
version: 1.7
description: Correctness of monoid multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-10-22
requires: bool
requires: list
requires: monoid-mult-add-def
requires: monoid-mult-def
requires: monoid-mult-thm
requires: monoid-witness
requires: natural
requires: natural-bits
show: "Algebra.Monoid"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

main {
  article: "monoid-mult-add-thm.art"
}
