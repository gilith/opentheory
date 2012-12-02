name: haskell-thm
version: 1.56
description: Properties of the Haskell base
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-11-13
requires: base
requires: haskell-def
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"

main {
  article: "haskell-thm.art"
}
