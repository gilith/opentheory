name: bool-choice
version: 1.0
description: Basic boolean theory relying on the axiom of choice
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"

bool-choice-exists {
  package: bool-choice-exists-1.0
}

bool-choice-select {
  import: bool-choice-exists
  package: bool-choice-select-1.0
}

bool-choice-cases {
  import: bool-choice-exists
  package: bool-choice-cases-1.0
}

bool-choice-quant {
  import: bool-choice-cases
  package: bool-choice-quant-1.0
}

bool-choice-cond {
  import: bool-choice-select
  import: bool-choice-cases
  package: bool-choice-cond-1.0
}

bool-choice-skolem {
  import: bool-choice-exists
  import: bool-choice-cond
  package: bool-choice-skolem-1.0
}

bool-choice-induct {
  import: bool-choice-cases
  import: bool-choice-cond
  package: bool-choice-induct-1.0
}

main {
  import: bool-choice-exists
  import: bool-choice-select
  import: bool-choice-cases
  import: bool-choice-quant
  import: bool-choice-cond
  import: bool-choice-skolem
  import: bool-choice-induct
}
