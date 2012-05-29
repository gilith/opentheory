name: h-thm
version: 1.75
description: Proof of memory safety for the H API
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-05-29
requires: base
requires: h-def
requires: word10
show: "Data.Bool"
show: "Data.Byte"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word10"
show: "Data.Word12"
show: "Function"
show: "Number.Natural"
show: "Set"
show: "System.H"

main {
  article: "h-thm.art"
}
