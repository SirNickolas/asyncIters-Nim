import std/unittest
import asyncIters

test "can await an iterator":
  var s = 0
  for i in awaitIter 0 .. 3:
    s += i
  check s == 6
