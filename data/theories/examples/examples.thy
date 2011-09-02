name: examples
version: 1.26
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.12
}

word10 {
  package: word10-1.11
}

word12 {
  package: word12-1.10
}

word16 {
  import: byte
  package: word16-1.14
}

parser {
  package: parser-1.20
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.22
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.15
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
