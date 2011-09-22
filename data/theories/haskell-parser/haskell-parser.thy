name: haskell-parser
version: 1.12
description: Basic parsers in Haskell
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
  interpret: const "Parser.destParser" as "Haskell.Parser.unParser"
  interpret: const "Parser.mkParser" as "Haskell.Parser.Parser"
  interpret: const "Parser.Stream.append" as "Haskell.Parser.Stream.append"
  interpret: const "Parser.Stream.fromList" as "Haskell.Parser.Stream.fromList"
  interpret: const "Parser.Stream.length" as "Haskell.Parser.Stream.size"
  interpret: const "Parser.Stream.toList" as "Haskell.Parser.Stream.toList"
  interpret: const "Parser.Stream.eof" as "Haskell.Parser.Stream.Eof"
  interpret: const "Parser.Stream.error" as "Haskell.Parser.Stream.Error"
  interpret: const "Parser.Stream.stream" as "Haskell.Parser.Stream.Stream"
  interpret: const "Parser.Stream.case" as "Haskell.Parser.Stream.Stream.case"
  package: parser-1.21
}

src {
  import: thm
  package: haskell-parser-src-1.9
}

test {
  import: thm
  package: haskell-parser-test-1.3
}

main {
  import: thm
  import: src
  import: test
}
