import std/macros

macro awaitIter*(loop: ForLoopStmt) =
  ## Iterate over an async iterator. Like regular `await`, this can only be done in procedures
  ## marked with `{.async.}` or `{.asyncIter.}`.

  let invocation = loop[1]
  invocation.expectLen 2
  loop[1] = invocation[1]
  loop
