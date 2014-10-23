name: natural-prime
version: 1.62
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
  package: natural-prime-def-1.42
  checksum: 3f983669c8f9862263e3da642cc3942eae24c185
}

thm {
  import: def
  package: natural-prime-thm-1.50
  checksum: 12569d67ba37b50ce8a5afe75ae4dfa1a43a5775
}

stream {
  import: thm
  package: natural-prime-stream-1.24
  checksum: 3be4788611161db0d89a020449de9f201ff82d58
}

sieve {
  import: stream
  package: natural-prime-sieve-1.25
  checksum: e7e338671704683010724342c70b16cf30796529
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
