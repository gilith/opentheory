name: natural-prime-stream-thm
version: 1.4
description: Properties of the ordered stream of all prime numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-06-16
requires: bool
requires: list
requires: natural
requires: natural-prime-stream-def
requires: natural-prime-thm
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Stream"
show: "Number.Natural"

main {
  article: "natural-prime-stream-thm.art"
}