version     = "1.3.1"
author      = "Nickolay Bukreyev"
description = "Async iterators. Able to both await futures and yield values"
license     = "MIT"

srcDir = "src"

requires(
  "nim >= 1.4.0",
  # "chronos", # Just for running tests with it.
  "letUtils >= 1.1.1 & < 2.0.0",
)
