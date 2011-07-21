name: haskell-parser
version: 1.0
description: Parser ready to be synthesized as Haskell
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

thm {
  interpret: type "Parser.Stream.stream" as "Haskell.Parser.Stream.Stream"
  interpret: const "Parser.Stream.append" as "Haskell.Parser.Stream.append"
  interpret: const "Parser.Stream.case" as "Haskell.Parser.Stream.case"
  interpret: const "Parser.Stream.eof" as "Haskell.Parser.Stream.Eof"
  interpret: const "Parser.Stream.error" as "Haskell.Parser.Stream.Error"
  interpret: const "Parser.Stream.fromList" as "Haskell.Parser.Stream.fromList"
  interpret: const "Parser.Stream.isProperSuffix" as "Haskell.Parser.Stream.isProperSuffix"
  interpret: const "Parser.Stream.isSuffix" as "Haskell.Parser.Stream.isSuffix"
  interpret: const "Parser.Stream.length" as "Haskell.Parser.Stream.length"
  interpret: const "Parser.Stream.stream" as "Haskell.Parser.Stream.Stream"
  interpret: const "Parser.Stream.toList" as "Haskell.Parser.Stream.toList"
  package: parser-1.18
}

src {
  import: thm
  package: haskell-parser-src-1.0
}

main {
  import: src
}
