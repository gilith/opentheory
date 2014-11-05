name: natural-prime-sieve
version: 1.28
description: The sieve of Eratosthenes
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: natural-divides
requires: natural-prime-stream
requires: pair
requires: relation
requires: stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Number.Natural.Prime.Sieve"

def {
  package: natural-prime-sieve-def-1.31
  checksum: acbb74379f13f269f25dc7541bba50542d8f9ab9
}

thm {
  import: def
  package: natural-prime-sieve-thm-1.28
  checksum: 2aabc4903d624c9d7b22d2d55e58bab968447a0f
}

main {
  import: def
  import: thm
}
