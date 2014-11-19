name: natural-prime
version: 1.67
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
  package: natural-prime-stream-1.28
  checksum: f4b892609c1374dfeee147fd5b2eb2e247d774e9
}

sieve {
  import: stream
  package: natural-prime-sieve-1.29
  checksum: a4d100ace2564ab08c773630c66a02e3c79e47e3
}

main {
  import: def
  import: thm
  import: stream
  import: sieve
}
