name: bool-choice-cond
version: 1.0
description: Theorems about the conditional relying on the axiom of choice
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

bool-choice-cond-alt {
  package: bool-choice-cond-alt-1.0
}

bool-choice-cond-thm {
  import: bool-choice-cond-alt
  package: bool-choice-cond-thm-1.0
}

main {
  import: bool-choice-cond-alt
  import: bool-choice-cond-thm
}
