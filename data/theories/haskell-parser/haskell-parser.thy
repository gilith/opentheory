name: haskell-parser
version: 1.18
description: Simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Haskell.Parser"
show: "Number.Natural"

thm {
  interpret: type "Parser.parser" as "Haskell.Parser.Parser"
  interpret: type "Parser.Stream.stream" as "Haskell.Parser.Stream.Stream"
  interpret: const "Parser.parse" as "Haskell.Parser.parse"
  interpret: const "Parser.parseAll" as "Haskell.Parser.parseAll"
  interpret: const "Parser.parseAll.pa" as "Haskell.Parser.parseAll.pa"
  interpret: const "Parser.map" as "Haskell.Parser.parseMap"
  interpret: const "Parser.parseOption" as "Haskell.Parser.parseMaybe"
  interpret: const "Parser.parseNone" as "Haskell.Parser.parseNone"
  interpret: const "Parser.parseNone.pn" as "Haskell.Parser.parseNone.pn"
  interpret: const "Parser.parsePair" as "Haskell.Parser.parsePair"
  interpret: const "Parser.parsePair.pbc" as "Haskell.Parser.parsePair.pbc"
  interpret: const "Parser.partialMap" as "Haskell.Parser.parsePartialMap"
  interpret: const "Parser.partialMap.pf" as "Haskell.Parser.parsePartialMap.pf"
  interpret: const "Parser.parseSome" as "Haskell.Parser.parseSome"
  interpret: const "Parser.parseStream" as "Haskell.Parser.parseStream"
  interpret: const "Parser.destParser" as "Haskell.Parser.unParser"
  interpret: const "Parser.mkParser" as "Haskell.Parser.Parser"
  interpret: const "Parser.Stream.append" as "Haskell.Parser.Stream.append"
  interpret: const "Parser.Stream.case.error.eof.stream" as "Haskell.Parser.Stream.case.Error.Eof.Stream"
  interpret: const "Parser.Stream.fromList" as "Haskell.Parser.Stream.fromList"
  interpret: const "Parser.Stream.length" as "Haskell.Parser.Stream.size"
  interpret: const "Parser.Stream.toList" as "Haskell.Parser.Stream.toList"
  interpret: const "Parser.Stream.eof" as "Haskell.Parser.Stream.Eof"
  interpret: const "Parser.Stream.error" as "Haskell.Parser.Stream.Error"
  interpret: const "Parser.Stream.stream" as "Haskell.Parser.Stream.Stream"
  package: parser-1.22
}

src {
  import: thm
  package: haskell-parser-src-1.14
}

test {
  import: thm
  package: haskell-parser-test-1.4
}

main {
  import: thm
  import: src
  import: test
}
