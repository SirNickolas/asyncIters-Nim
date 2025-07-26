# Async iterators for Nim

```nim
from   std/asyncdispatch import sleepAsync, waitFor
import asyncIters # Imports `async`, `await`, and `std/asyncfutures` as well.

func countUpAsync(a, b: int): AsyncIterator[int] =
  iterator countUpAsync: Future[int] {.asyncIter.} =
    for i in a .. b:
      echo "Generating..."
      await sleepAsync 50 # You can await.
      yieldAsync i        # And you can yield.

  result = countUpAsync

proc test {.async.} =
  for i in awaitIter countUpAsync(1, 5):
    echo "Received ", i
    await sleepAsync 150

waitFor test()
```

[API documentation](https://sirnickolas.github.io/asyncIters-Nim/asyncIters)

`yieldAsync` passes values back to the caller. Sadly, we could not use the obvious `yield` keyword
because it is reserved in async procedures to mean, “wait for a future to finish but do not perform
error handling.”

`yieldAsyncFrom` allows to delegate iteration to another async iterator. It is semantically
equivalent to  
`for x in awaitIter another: yieldAsync x` but is more efficient. Example:

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


## `asyncIters` vs `asyncstreams`

[`std/asyncstreams`][asyncstreams] may look similar to this library, but they solve different
problems. Async procedures communicating via a `FutureStream` run as independently as possible.
Sometimes this is the right thing, but sometimes you want finer control. For example, a consumer
might decide to abort iteration, and it would like to stop the producer as well. Moreover, it is
important to stop it immediately so that no extraneous data is produced. In this case,
`FutureStream` is a bad solution. On the other hand, `asyncIters` were designed with this scenario
in mind.

[asyncstreams]: https://nim-lang.org/docs/asyncstreams.html


## Using with `chronos/asyncloop`

This library is mostly compatible with [Chronos][], with a single exception. You cannot `return`
from an `awaitIter` loop — it produces a compilation error. As a workaround, consider assigning
to `result` and `break`ing from the loop. (Hint: you can wrap the whole body of your procedure
in a [labeled][block-stmt] `block` statement and break out of it.)

Upstream issue: [status-im/nim-chronos#368][].

For Chronos **4.x**, you need to pass `-d=chronosHandleException` flag to the compiler. It is
a temporary workaround; this will be dealt with in a future version of `asyncIters`.

And if you are using Chronos with Nim **1.x**, there’s one more gotcha to be aware of:

<details>
You cannot use the pragma syntax with `asyncIter`.

```nim
# These don't work.
iterator myIter: Future[int] {.asyncIter.} =
  discard

let myAnonIter = iterator: Future[int] {.asyncIter.} =
  discard

# Use these instead:
asyncIter:
  iterator myIter: Future[int] =
    discard

let myAnonIter = asyncIter(iterator: Future[int] =
  discard
)
```

That was a compiler bug: [status-im/nim-chronos#367][].
</details>

[Chronos]: https://github.com/status-im/nim-chronos
[status-im/nim-chronos#367]: https://github.com/status-im/nim-chronos/issues/367
[status-im/nim-chronos#368]: https://github.com/status-im/nim-chronos/issues/368
[block-stmt]: https://nim-lang.org/docs/manual.html#statements-and-expressions-block-statement


## How it works

`asyncIter` transforms the iterator definition to an async proc (which, ironically, will be
eventually transformed by `{.async.}` back to an iterator):

```nim
iterator countToTen: Future[int] {.asyncIter.} =
  for i in 0 ..< 10:
    yieldAsync i

# =>

proc countToTen(body: proc (item: int): Future[uint32] {.gcSafe.}): Future[uint32] {.async.} =
  for i in 0 ..< 10:
    if (let ret = await body i; ret != 0'u32):
      return ret
```

`awaitIter` transforms the loop to an async proc as well (loop variables become procedure’s
parameters) and calls the provided iterator with it:

```nim
for item in awaitIter countToTen:
  echo item

# =>

proc asyncForBody(item: int): Future[uint32] {.async.} =
  echo item

discard await countToTen asyncForBody
```


### What are `Future[uint32]` for?

For supporting `break` and `return`. A more complex example:

```nim
block blk:
  for item in awaitIter countToTen:
    break
    break blk
    return item

# =>

block blk:
  proc asyncForBody(item: int): Future[uint32] {.async.} =
    return 1'u32 # `break`
    return 3'u32 # `break blk`
    complete retFuture, item # It is the future of the *outer* proc.
    return 2'u32 # `return item`

  let ret = await countToTen asyncForBody
  # Recall that `countToTen` stops iteration upon receiving a non-zero.
  case ret:
    of 0'u32, 1'u32:
      discard
    of 2'u32:
      return nil # This is actually generated by `{.async.}`; we just reattach it here.
    else:
      break blk
```

If you are curious, the idea of this transformation originates from the [D language][op-apply].

[op-apply]: https://dlang.org/spec/statement.html#foreach_over_struct_and_classes


## Limitations

1.  With regular Nim iterators, you supply arguments on each step:

    ```nim
    # Not async.
    iterator double(n: int): int {.closure.} = # `{.inline.}` works too.
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
    I’d like to add this feature, but it requires reimplementing [`asyncdispatch.async`][asyncmacro]
    from scratch — that’s an interesting task, but not today, sorry.

    [asyncmacro]: https://github.com/nim-lang/Nim/blob/version-1-6/lib/pure/asyncmacro.nim

2.  In regular `{.async.}` procedures, you must not invoke templates or macros that contain
    a `return` statement:

    ```nim
    template returnIfNegative(x: int) =
      if x < 0:
        return

    proc process(x: int) {.async.} =
      returnIfNegative x # WRONG.
    ```

    With async iterators, this restriction goes further:

    1. You must not *indirectly* (i.e., via a template) invoke `return`, `break`, or `continue`
       from inside an `awaitIter` loop body.
    2. You must not *indirectly* access the `result` implicit variable from inside an `awaitIter`
       loop body.

3.  `awaitIter` is always tied to a `for` loop. I.e., you cannot pull a single value from
    an iterator; you can only run through all values it is going to produce. However, `break`ing
    is allowed, as well as iterating multiple times, so you can work around it.

4.  [`multisyncIter`][multisync] is not currently implemented.

    [multisync]: https://nim-lang.org/docs/asyncdispatch.html#multisync.m,untyped
