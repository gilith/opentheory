name: examples
version: 1.46
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"

byte {
  package: byte-1.25
}

word10 {
  package: word10-1.24
}

word12 {
  package: word12-1.23
}

word16 {
  import: byte
  package: word16-1.29
}

parser {
  package: parser-1.32
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.35
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.31
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
