##[
  This package implements asynchronous iterators. For more information about asynchronous procedures
  in general, see `std/asyncdispatch`_ documentation.

  .. _std/asyncdispatch: https://nim-lang.org/docs/asyncdispatch.html
]##
runnableExamples:
  # `async`, `await`, and `std/asyncfutures` are imported as well.
  from   std/asyncdispatch import sleepAsync, waitFor

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

from ./asyncIters/cfg import backend

template exportWhenDeclared(symbol: untyped) {.used.} =
  when declared symbol:
    export symbol

when backend == "asyncdispatch":
  from std/asyncdispatch import nil
  from std/asyncfutures import Future

  export asyncdispatch.async
  exportWhenDeclared asyncdispatch.await
  export asyncfutures except callSoon # `asyncdispatch` provides an alternative implementation.
elif backend == "chronos":
  from chronos/asyncloop as chr import Future

  # Export the bare minimum necessary to compile async procedures with older `chronos` versions,
  # plus the shiny new `chronos/futures` API if it is available.
  export Future, chr.FutureBase, chr.async, chr.complete, chr.newFuture
  exportWhenDeclared chr.await
  exportWhenDeclared chr.futureContinue
  exportWhenDeclared chr.internalCheckComplete
  exportWhenDeclared chr.internalRead
  exportWhenDeclared chr.futures # https://github.com/status-im/nim-chronos/pull/405

when defined nimdoc:
  {.push.}
  when (NimMajor, NimMinor) >= (1, 6):
    {.hint[DuplicateModuleImport]: off.}
  include ./asyncIters/private/asyncIter
  include ./asyncIters/private/awaitIter
  {.pop.}
else:
  from ./asyncIters/private/asyncIter import nil
  from ./asyncIters/private/awaitIter import customAsyncIterator

  export asyncIter, awaitIter

when declared Future:
  when (NimMajor, NimMinor) >= (1, 9):
    {.warning[AmbiguousLink]: off.} # `customAsyncIterator`
  template the(x): untyped = x # https://github.com/SirNickolas/asyncIters-Nim/issues/1
  type AsyncIterator*[T] = the customAsyncIterator(T, Future)
    ##[
      Type of async iterators after they are processed.

      This type is not declared if you pass `-d=asyncBackend:none`:option: (or some unrecognized
      backend name) to the compiler. Known backends include `asyncdispatch`_ (used by default
      if not set explicitly) and `chronos`_. If you’d like to use `asyncIters` with a backend that
      did not exist at the moment of writing, you need to use `customAsyncIterator`_ and specify
      some `Future`_-like type.

      Note also that this is only a *suggested* iterator type. Nothing stops you from using
      a different one or even having multiple in the same program.

      .. _asyncdispatch: https://nim-lang.org/docs/asyncdispatch.html
      .. _chronos: https://github.com/status-im/nim-chronos
      .. _customAsyncIterator: #customAsyncIterator.t,typed,typed
      .. _Future: https://nim-lang.org/docs/asyncfutures.html
    ]##
