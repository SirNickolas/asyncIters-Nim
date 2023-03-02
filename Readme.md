# Async iterators for Nim

```nim
from   std/asyncdispatch import sleepAsync, waitFor
import asyncIters # Imports `async` and `std/asyncfutures` as well.

func countUpAsync(a, b: int): AsyncIterator[int] =
  result = iterator: Future[int] {.asyncIter.} =
    for i in a .. b:
      echo "Generating..."
      await sleepAsync 50 # You can await.
      yieldAsync i        # And you can yield.

proc test {.async.} =
  for i in awaitIter countUpAsync(1, 5):
    echo "Received ", i
    await sleepAsync 150

waitFor test()
```

Because `yield` in async procedures is reserved to mean, “wait for a future to finish but do not
perform error handling,” we had to introduce a new control structure, `yieldAsync`. Its
counterpart `yieldAsyncFrom` allows to delegate iteration to another async iterator:

```nim
func countUpAsync(a, b: int; step = 1): auto =
  result = iterator: Future[int] {.asyncIter.} =
    for i in countUp(a, b, step):
      yieldAsync i

func evensAndOdds(a, b: int): auto =
  let evens = countUpAsync(a, b, 2)
  let odds  = countUpAsync(a + 1, b, 2)
  result = iterator: Future[int] {.asyncIter.} =
    yieldAsyncFrom evens
    yieldAsyncFrom odds

proc test {.async.} =
  for x in awaitIter evensAndOdds(0, 9):
    echo x # => 0 2 4 6 8 1 3 5 7 9
```


## Limitations

* With regular Nim iterators, you supply arguments on each step:

  ```nim
  iterator double(n: int): int {.closure.} = # `{.inline.}` works, too.
    while true:
      yield n shl 1

  var prev = 0
  for cur in double prev + 1:
    echo cur
    if cur > 100:
      break
    prev = cur
  # => 2 6 14 30 62 126
  ```

  Generators in Python and JavaScript (both sync and async) work the same: you can pass data both
  in and out. They just use a different syntax:

  ```py
  def double(n):
      while True:
          n = yield n << 1

  g = double(1)
  cur = next(g)
  while True:
      print(cur)
      if cur > 100:
          break
      cur = g.send(cur + 1)
  ```

  Unfortunately, async iterators implemented in this library do not support such usage pattern.
  Parameterized iterators are not allowed. You can provide arguments only at the start, before
  iteration begins, by wrapping the iterator in a closure (see the synopsis for an example).
  I’d like to add this feature, but it requires reimplementing [`asyncdispatch.async`][async]
  from scratch — that’s an interesting task, but not today, sorry.

  [async]: https://github.com/nim-lang/Nim/blob/version-1-6/lib/pure/asyncmacro.nim

* In regular `{.async.}` procedures, you must not invoke templates or macros that contain a `return`
  statement:

  ```nim
  template returnIfNegative(x: int) =
    if x < 0:
      return

  proc process(x: int) {.async.} =
    returnIfNegative x # WRONG.
  ```

  With async iterators, this restriction goes further:

  1. You must not **indirectly** invoke `yieldAsync` and `yieldAsyncFrom` (they are magic
     identifiers so the compiler won’t let you).
  2. You must not **indirectly** invoke `return`, `break`, or `continue` from inside an `awaitIter`
     loop body.
  3. In an `awaitIter` loop body, you must not access the `result` implicit variable.

* `awaitIter` is always tied to a `for` loop. I.e., you cannot pull a single value from an iterator;
  you can only run through all values it is going to produce. However, `break`ing is allowed,
  as well as iterating multiple times, so you can work around it.
