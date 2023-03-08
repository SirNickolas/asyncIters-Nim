from   std/asyncdispatch import waitFor
import std/unittest
import asyncIters

template runAsync(body: untyped) =
  proc run {.gensym, async.} = body
  waitFor run()

test "can declare an async iterator":
  iterator named0: Future[int] {.asyncIter, used.} = discard
  iterator named1(): Future[int] {.used, asyncIter.} = discard
  # Anonymous iterators (and procedures) produce unhelpful stack traces. But they are supported
  # if you prefer conciseness over ease of debugging.
  let unnamed0 {.used.} = iterator: Future[int] {.asyncIter.} = discard
  let unnamed1 {.used.} = iterator (): Future[int] {.asyncIter.} = discard

test "can iterate over an async iterator":
  iterator produce2: Future[int] {.asyncIter.} =
    yieldAsync 2
    yieldAsync 2

  var n = 0
  runAsync:
    for x in awaitIter produce2:
      check x == 2
      n += 1
  check n == 2

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
  check sum == 10

func countUpAsync(a, b: int; step = 1): auto =
  ## A simple iterator for tests. Like `countUp`, but pretends to be asynchronous.

  result = iterator: Future[int] {.asyncIter.} =
    for i in countUp(a, b, step):
      yieldAsync i

test "can yield from another async iterator":
  func evensAndOdds(a, b: int): auto =
    let evens = countUpAsync(a, b, 2)
    let odds  = countUpAsync(a + 1, b, 2)
    result = iterator: Future[int] {.asyncIter.} =
      yieldAsyncFrom evens
      yieldAsyncFrom odds

  var data: seq[int]
  runAsync:
    for x in awaitIter evensAndOdds(0, 9):
      data &= x
  check data == [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]

test "`awaitIter` can unpack simple tuples":
  iterator indexedStrings: Future[(int, string)] {.asyncIter.} =
    yieldAsync 1, "test"
    yieldAsync (1, "test")

  iterator indexedPositions: Future[(int, string, tuple[x, y: float])] {.asyncIter.} =
    yieldAsync 1, "here", (2.0, 4.0)
    yieldAsync (1, "here", (2.0, 4.0))

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
    for i, s, (x, y) in awaitIter indexedPositions:
      check i == 1
      check s == "here"
      check x == 2.0
      check y == 4.0
      n += 1
  check n == 8

test "`awaitIter` can unpack a 1-element tuple":
  iterator wrap5: Future[(int, )] {.asyncIter.} =
    check not compiles yieldAsync 5
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

test "can declare procedures inside a loop":
  var sum = 0
  runAsync:
    for i in awaitIter countUpAsync(1, 3):
      func sqr(x: int): int = return x * x # `return` is essential for this test.
      sum += i.sqr
  check sum == 1 + 4 + 9

test "can `continue` a loop":
  var sum = 0
  runAsync:
    for i in awaitIter countUpAsync(1, 10):
      if (i and 0x3) == 0x0:
        continue
      sum += i
    sum += 100
  check sum == (1 + 2 + 3) + (5 + 6 + 7) + 9 + 10 + 100

test "can `break` from a loop":
  var sum = 0
  runAsync:
    for i in awaitIter countUpAsync(1, 10):
      if (i and 0x3) == 0x0:
        break
      sum += i
    sum += 100
  check sum == 1 + 2 + 3 + 100

test "can `return` from a loop":
  var sum = 0
  runAsync:
    for i in awaitIter countUpAsync(1, 10):
      if (i and 0x3) == 0x0:
        return # From `runAsync`.
      sum += i
    check false
  check sum == 1 + 2 + 3

test "can `return` a value from a loop":
  var sum = 0

  proc run: Future[int] {.async.} =
    for i in awaitIter countUpAsync(1, 10):
      if (i and 0x3) == 0x0:
        return 13
      sum += i
    check false

  check waitFor(run()) == 13
  check sum == 1 + 2 + 3

test "can access `result` inside a loop":
  var t = (result: 0)

  proc run: Future[int] {.async.} =
    for i in awaitIter countUpAsync(1, 3):
      t.result += i
      result -= i

  check waitFor(run()) == -(1 + 2 + 3)
  check t == (1 + 2 + 3, )

test "can nest loops":
  var sum = 0

  proc run: Future[string] {.async.} =
    for i in awaitIter countUpAsync(1, 10):
      for j in awaitIter countUpAsync(i, 10):
        if j == 6:
          break
        elif i == 7 and j == 9:
          return "ok"
        sum += j
      sum += i * 100
    check false

  check waitFor(run()) == "ok"
  check sum == 2170

test "can `break` from a named block":
  var sum = 0
  runAsync:
    block outer:
      for i in awaitIter countUpAsync(1, 10):
        block inner:
          for j in awaitIter countUpAsync(i, 10):
            if j == 6:
              break inner
            elif i == 7 and j == 9:
              break outer
            sum += j
        sum += i * 100
      check false
  check sum == 2170

test "named blocks can shadow one another":
  var sum = 0
  runAsync:
    block blk:
      for i in awaitIter countUpAsync(1, 5):
        block blk:
          if i == 3:
            break blk
          sum += i
        if i == 4:
          break blk
        sum += i * 10
      check false
  check sum == 67

test "can refer to a block before it is shadowed":
  var sum = 0
  runAsync:
    block blk:
      for i in awaitIter countUpAsync(1, 5):
        if i == 4:
          break blk
        block blk:
          if i == 3:
            break blk
          sum += i
        sum += i * 10
      check false
  check sum == 63
