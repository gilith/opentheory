name: examples
version: 1.47
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"

byte {
  package: byte-1.26
}

word10 {
  package: word10-1.25
}

word12 {
  package: word12-1.24
}

word16 {
  import: byte
  package: word16-1.30
}

parser {
  package: parser-1.33
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.36
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.32
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
