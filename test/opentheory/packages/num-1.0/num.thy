name: num
version: 1.0
description: Basic theory of natural numbers
author: Joe Hurd <joe@gilith.com>
license: PublicDomain

require num-def {
  package: num-def-1.0
}

require numeral-def {
  import: num-def
  package: numeral-def-1.0
}

require add-def {
  import: num-def
  import: numeral-def
  package: add-def-1.0
}

theory {
  import num-def;
  import numeral-def;
  import add-def;
}
