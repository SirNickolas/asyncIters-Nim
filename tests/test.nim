import std/macros
import std/unittest
import asyncIters
from   asyncIters/cfg as asyncCfg import nil
from   ./uSelectiveImport import nil

const chronosBackend = asyncCfg.backend == "chronos"

when chronosBackend:
  from chronos/asyncloop import waitFor
else:
  from std/asyncdispatch import waitFor

template runAsync(body: untyped) =
  proc run {.genSym, async.} = body
  waitFor run()

proc main =
  test "can declare an async iterator":
    when NimMajor >= 2 or not chronosBackend: # https://github.com/status-im/nim-chronos/issues/367
      iterator named0: Future[int] {.asyncIter, used.} = discard
      iterator named1(): Future[int] {.used, asyncIter.} = discard
      # Anonymous iterators (and procedures) produce unhelpful stack traces. But they are supported
      # if you prefer conciseness over ease of debugging.
      let unnamed0 {.used.} = iterator: Future[int] {.asyncIter.} = discard
      let unnamed1 {.used.} = iterator (): Future[int] {.asyncIter.} = discard
    let unnamed2 {.used.} = asyncIter(iterator: Future[int] = discard)
    let unnamed3 {.used.} = asyncIter:
      (iterator: Future[int] = discard)

  test "can declare an async iterator inside a template":
    template declareIter =
      asyncIter:
        iterator nop: Future[int] {.used.} = discard

    declareIter

  test "can iterate over an async iterator":
    asyncIter:
      iterator produce2: Future[int] =
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
      asyncIter:
        iterator countUpTo: Future[int] =
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
    result = asyncIter(iterator: Future[int] =
      for i in countUp(a, b, step):
        yieldAsync i
    )

  test "can yield from another async iterator":
    func evensAndOdds(a, b: int): auto =
      let evens = countUpAsync(a, b, 2)
      let odds  = countUpAsync(a + 1, b, 2)
      result = asyncIter(iterator: Future[int] =
        yieldAsyncFrom evens
        yieldAsyncFrom odds
      )

    var data: seq[int]
    runAsync:
      for x in awaitIter evensAndOdds(0, 9):
        data &= x
    check data == [0, 2, 4, 6, 8, 1, 3, 5, 7, 9]

  test "`awaitIter` can unpack simple tuples":
    asyncIter:
      iterator indexedStrings: Future[(int, string)] =
        yieldAsync (1, "test")

      iterator indexedPositions: Future[(int, string, tuple[x, y: float])] =
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
    check n == 4

  test "`awaitIter` can unpack a 1-element tuple":
    asyncIter:
      iterator wrap5: Future[(int, )] =
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

  test "can yield from a template declared inside an async iterator":
    asyncIter:
      iterator produce1122: Future[int] =
        template yieldTwice(x: int) =
          yieldAsync x
          yieldAsync x

        yieldTwice 1
        yieldTwice 2

    var n = 0
    runAsync:
      for i in awaitIter produce1122:
        check i in 1 .. 2
        n += 1
    check n == 4

  test "can yield from a template declared outside an async iterator":
    template yieldTwice(x: int) =
      yieldAsync x
      yieldAsyncFrom countUpAsync(x, x)

    asyncIter:
      iterator produce1122: Future[int] =
        yieldTwice 1
        yieldTwice 2

    var n = 0
    runAsync:
      for i in awaitIter produce1122:
        check i in 1 .. 2
        n += 1
    check n == 4

  test "can redeclare `yieldAsync`":
    asyncIter:
      iterator nop: Future[int] =
        template yieldAsync(x: typed) = discard

        yieldAsync 1

    runAsync:
      for i in awaitIter nop:
        check false

  test "can continue from a template declared inside a loop":
    var n = 0
    runAsync:
      for i in awaitIter countUpAsync(1, 5):
        template continueIfSmall(x: int) =
          if x <= 2:
            continue

        continueIfSmall i
        n += 1
    check n == 3

  test "can continue a loop":
    var sum = 0
    runAsync:
      for i in awaitIter countUpAsync(1, 10):
        if (i and 0x3) == 0x0:
          continue
        sum += i
      sum += 100
    check sum == (1 + 2 + 3) + (5 + 6 + 7) + 9 + 10 + 100

  test "can break from a loop":
    var sum = 0
    runAsync:
      for i in awaitIter countUpAsync(1, 10):
        if (i and 0x3) == 0x0:
          break
        sum += i
      sum += 100
    check sum == 1 + 2 + 3 + 100

  test "can return from a loop":
    when chronosBackend:
      skip # https://github.com/status-im/nim-chronos/issues/368
    else:
      var sum = 0
      runAsync:
        for i in awaitIter countUpAsync(1, 10):
          if (i and 0x3) == 0x0:
            return # From `runAsync`.
          sum += i
        check false
      check sum == 1 + 2 + 3

  test "can return a value from a loop":
    when chronosBackend:
      skip # https://github.com/status-im/nim-chronos/issues/368
    else:
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

  test "can pass `result` from a loop to a template":
    template add5(x: int) = x += 5

    proc run: Future[int] {.async.} =
      for i in awaitIter countUpAsync(1, 2):
        result.add5
        add5 result

    check waitFor(run()) == 20

  test "can nest a regular loop inside async one":
    var sum = 0

    proc run: Future[string] {.async.} =
      block procBody:
        for i in awaitIter countUpAsync(1, 10):
          for j in i .. 10:
            if j == 6:
              break
            elif i == 7 and j == 9:
              when chronosBackend:
                result = "ok"
                break procBody
              else:
                return "ok"
            sum += j
            continue
          sum += i * 100
        check false

    check waitFor(run()) == "ok"
    check sum == 2170

  test "can nest loops":
    var sum = 0

    proc run: Future[string] {.async.} =
      block procBody:
        for i in awaitIter countUpAsync(1, 10):
          for j in awaitIter countUpAsync(i, 10):
            if j == 6:
              break
            elif i == 7 and j == 9:
              when chronosBackend:
                result = "ok"
                break procBody
              else:
                return "ok"
            sum += j
            continue
          sum += i * 100
        check false

    check waitFor(run()) == "ok"
    check sum == 2170

  test "can return an implicit `result` from a nested loop":
    when chronosBackend:
      skip # https://github.com/status-im/nim-chronos/issues/368
    else:
      var sum = 0

      proc run: Future[string] {.async.} =
        result = "ok"
        for i in awaitIter countUpAsync(1, 10):
          for j in awaitIter countUpAsync(i, 10):
            if i == 3 and j == 5:
              return
            sum += j
          sum += i * 100
        check false

      check waitFor(run()) == "ok"
      check sum == 416

  test "can break from a named block":
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

  test "unlabeled `break` does not interfere with `block`":
    var ok = false
    runAsync:
      for i in awaitIter countUpAsync(1, 5):
        block:
          break # Behaviour is different from Nim 1.x!
        check false
      for i in awaitIter countUpAsync(1, 5):
        block blk:
          break # Behaviour is different from Nim 1.x!
        check false
      ok = true
    check ok

  test "can have `proc` as a direct child of a loop":
    # Usually, an `nnkForStmt` wraps an `nnkStmtList`. We have to write a macro to attach
    # an `nnkProcDef` directly.
    macro construct(iter: typed) =
      result = quote:
        for i in awaitIter `iter`:
          proc p: string {.used.} = return "unused" # `return` is essential for this test.
      result[2].expectKind nnkStmtList
      result[2].expectLen 1
      result[2] = result[2][0]

    var ok = false
    runAsync:
      construct countUpAsync(1, 5)
      ok = true
    check ok

  test "can have `break` as a direct child of a `block`":
    # Usually, an `nnkBlockStmt` wraps an `nnkStmtList`. We have to write a macro to attach
    # an `nnkBreakStmt` directly.
    macro construct(n: int; iter: typed) =
      result = quote:
        for i in awaitIter `iter`:
          block blk:
            break blk
          `n` += 1
      let blk = result[2][0]
      blk.expectKind nnkBlockStmt
      blk[1].expectKind nnkStmtList
      blk[1].expectLen 1
      blk[1] = blk[1][0]

    var n = 0
    runAsync:
      construct n, countUpAsync(1, 5)
    check n == 5

  test "customAsyncIterator can work with arbitrary type constructors":
    type ArrayTypeCtor = object
      len: int

    template `[]`(ctor: ArrayTypeCtor; T: type): type =
      array[ctor.len, T]

    const arr10 = ArrayTypeCtor(len: 10)
    check (arr10[int] is array[10, int]) # Nim 1.4 requires parenthizing the expression.
    check (customAsyncIterator(int, arr10) is (
      # An implementation detail.
      proc (body: proc (item: int): array[10, uint32]): array[10, uint32]
    ))

  test "can return different values from a nested loop":
    # This test requires a custom async backend:
    type Identity[T] = T
    template async {.pragma.}
    template await(x: typed): untyped = x

    asyncIter:
      iterator one: Identity[int] =
        yieldAsync 1

    proc run(flag: bool): string =
      for i in awaitIter one:
        check i == 1
        for j in awaitIter one:
          check j == 1
          if flag:
            return "yes"
          return "no" # Having two separate `return` statements is essential for this test.

    check run(true) == "yes"
    check run(false) == "no"

main()
