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

from ./asyncIters/private/backend import nil

export backend except asyncItersBackend

when defined nimdoc:
  include ./asyncIters/private/asyncIter
  include ./asyncIters/private/awaitIter
else:
  from ./asyncIters/private/asyncIter import nil
  from ./asyncIters/private/awaitIter import nil

  export asyncIter, awaitIter
