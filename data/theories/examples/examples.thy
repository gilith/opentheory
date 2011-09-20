name: examples
version: 1.28
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.13
}

word10 {
  package: word10-1.12
}

word12 {
  package: word12-1.11
}

word16 {
  import: byte
  package: word16-1.15
}

parser {
  package: parser-1.20
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.23
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.17
}

main {
  import: byte
  import: word10
  import: word12
  import: word16
  import: parser
  import: char
  import: h
}
