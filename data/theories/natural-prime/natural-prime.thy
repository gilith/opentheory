name: natural-prime
version: 1.54
description: Prime natural numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: natural-divides
requires: natural-gcd
requires: pair
requires: relation
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve" as "Sieve"

def {
  package: natural-prime-def-1.39
}

thm {
  import: def
  package: natural-prime-thm-1.46
}

stream {
  import: thm
  package: natural-prime-stream-1.18
}

sieve {
  import: stream
  package: natural-prime-sieve-1.17
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
