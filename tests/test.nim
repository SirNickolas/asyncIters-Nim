from   std/asyncdispatch import nil
import std/unittest
import asyncIters

template runAsync(body) =
  proc run {.gensym, async.} = body
  asyncdispatch.waitFor run()

test "can declare an async iterator":
  iterator named0: Future[int] {.asyncIter, used.} = discard
  iterator named1(): Future[int] {.used, asyncIter.} = discard
  # Anonymous iterators (and procedures) produce unhelpful stack traces. But they are supported
  # if you prefer conciseness over ease of debugging.
  let unnamed0 {.used.} = iterator: Future[int] {.asyncIter.} = discard
  let unnamed1 {.used.} = iterator (): Future[int] {.asyncIter.} = discard

test "can call `awaitIter` and `awaitEach`":
  iterator produce2: Future[int] {.asyncIter.} =
    yieldAsync 2
    yieldAsync 2

  var n = 0
  runAsync:
    for x in awaitIter produce2:
      check x == 2
      n += 1
    awaitEach produce2, x:
      check x == 2
      n += 1
  check n == 4

test "async iterator can close over variables in its scope":
  func countUpTo(n: int): AsyncIterator[int] =
    iterator countUpTo: Future[int] {.asyncIter.} =
      for i in 0 ..< n:
        yieldAsync i

    result = countUpTo

  var sum = 0
  runAsync:
    for x in awaitIter countUpTo 5:
      sum += x
    awaitEach countUpTo 5, x:
      sum += x
  check sum == 20

test "`awaitIter` can unpack simple tuples":
  iterator indexedStrings: Future[(int, string)] {.asyncIter.} =
    yieldAsync 1, "test"
    yieldAsync (1, "test")

  iterator indexedPositions: Future[(int, tuple[x, y: float])] {.asyncIter.} =
    yieldAsync 1, (2.0, 4.0)
    yieldAsync (1, (2.0, 4.0))

  var n = 0
  runAsync:
    for pair in awaitIter indexedStrings:
      check pair == (1, "test")
      n += 1
    for i, s in awaitIter indexedStrings:
      check i == 1
      check s == "test"
      n += 1
    for (i, s) in awaitIter indexedStrings:
      check i == 1
      check s == "test"
      n += 1
    for i, (x, y) in awaitIter indexedPositions:
      check i == 1
      check x == 2.0
      check y == 4.0
      n += 1
  check n == 8

test "`awaitIter` can unpack a 1-element tuple":
  iterator wrap5: Future[(int, )] {.asyncIter.} =
    yieldAsync (5, )

  var n = 0
  runAsync:
    for wrapped in awaitIter wrap5:
      check wrapped == (5, )
      n += 1
    for (five) in awaitIter wrap5:
      check five == 5
      n += 1
    for (five, ) in awaitIter wrap5:
      check five == 5
      n += 1
  check n == 3

test "`awaitEach` can unpack deeply nested tuples":
  iterator it: Future[((string, int), float, (seq[string], (bool, )))] {.asyncIter.} =
    yieldAsync ("a", 1), 2.0, (@["b", "c"], (true, ))

  var n = 0
  runAsync:
    awaitEach it, t:
      check t == (("a", 1), 2.0, (@["b", "c"], (true, )))
      n += 1
    awaitEach it, a, b, c:
      check a == ("a", 1)
      check b == 2.0
      check c == (@["b", "c"], (true, ))
      n += 1
    awaitEach it, (a, b, c):
      check a == ("a", 1)
      check b == 2.0
      check c == (@["b", "c"], (true, ))
      n += 1
    awaitEach it, (a, b), c, (d, e):
      check a == "a"
      check b == 1
      check c == 2.0
      check d == ["b", "c"]
      check e == (true, )
      n += 1
    awaitEach it, (a, b), c, (d, (e)):
      check a == "a"
      check b == 1
      check c == 2.0
      check d == ["b", "c"]
      check e == true
      n += 1
    awaitEach it, (a, b), c, (d, (e, )):
      check a == "a"
      check b == 1
      check c == 2.0
      check d == ["b", "c"]
      check e == true
      n += 1
  check n == 6
