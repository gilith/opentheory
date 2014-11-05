name: natural-prime
version: 1.65
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
  package: natural-prime-def-1.43
  checksum: 7d7f0fefae5f3712431fdb6bd9689503b4102de5
}

thm {
  import: def
  package: natural-prime-thm-1.52
  checksum: c5aed1ace2f77d1cadd247f42960ee5b01f81736
}

stream {
  import: thm
  package: natural-prime-stream-1.27
  checksum: 1cc62f61ce664cc172f976a4d14fe4c075bac5a5
}

sieve {
  import: stream
  package: natural-prime-sieve-1.28
  checksum: 5b9eb6c2050125ff81e13e8ebbb238f673ed6cab
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
