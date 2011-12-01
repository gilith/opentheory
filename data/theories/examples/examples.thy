name: examples
version: 1.54
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"

byte {
  package: byte-1.31
}

word10 {
  package: word10-1.30
}

word12 {
  package: word12-1.29
}

word16 {
  import: byte
  package: word16-1.35
}

parser {
  package: parser-1.40
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.42
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.35
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
