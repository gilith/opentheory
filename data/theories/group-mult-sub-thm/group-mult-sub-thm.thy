name: group-mult-sub-thm
version: 1.3
description: Correctness of group multiplication by repeated subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-09-25
requires: bool
requires: group-def
requires: group-mult-def
requires: group-mult-sub-def
requires: group-mult-thm
requires: group-thm
requires: group-witness
requires: list
requires: natural
requires: natural-fibonacci
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

main {
  article: "group-mult-sub-thm.art"
}
