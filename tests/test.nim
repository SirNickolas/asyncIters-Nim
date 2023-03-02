import std/unittest
import asyncIters

test "can declare an async iterator":
  iterator named0: Future[int] {.asyncIter.} = discard
  iterator named1(): Future[int] {.asyncIter.} = discard
  let unnamed0 = iterator: Future[int] {.asyncIter.} = discard
  let unnamed1 = iterator (): Future[int] {.asyncIter.} = discard

test "can await an iterator":
  var s = 0
  for i in awaitIter 0 .. 3:
    s += i
  check s == 6
