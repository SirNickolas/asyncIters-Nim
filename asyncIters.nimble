version     = "1.1.0"
author      = "Nickolay Bukreyev"
description = "Async iterators. Able to both await futures and yield values"
license     = "MIT"

srcDir = "src"

requires(
  "nim >= 1.4.0",
  "letUtils >= 1.1.1 & < 2.0.0",
)
