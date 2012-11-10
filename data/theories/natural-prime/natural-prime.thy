name: natural-prime
version: 1.56
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
  package: natural-prime-def-1.40
}

thm {
  import: def
  package: natural-prime-thm-1.47
}

stream {
  import: thm
  package: natural-prime-stream-1.19
}

sieve {
  import: stream
  package: natural-prime-sieve-1.19
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
