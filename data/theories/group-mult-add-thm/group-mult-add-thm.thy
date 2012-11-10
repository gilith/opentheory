name: group-mult-add-thm
version: 1.10
description: Correctness of group multiplication by repeated addition
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-11-10
requires: bool
requires: group-mult-add-def
requires: group-mult-def
requires: group-mult-thm
requires: group-thm
requires: group-witness
requires: list
requires: natural
requires: natural-bits
show: "Algebra.Group"
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

main {
  article: "group-mult-add-thm.art"
}
