from std/strutils import normalize

const
  asyncBackend {.strDefine.} = "asyncdispatch"
  asyncItersBackend* = asyncBackend.normalize
    ## Use `-d=asyncBackend=<name>` to configure this constant.

when asyncItersBackend == "asyncdispatch":
  from std/asyncdispatch import async
  from std/asyncfutures import nil

  export async
  when declared asyncdispatch.await:
    export asyncdispatch.await
  export asyncfutures except callSoon # `asyncdispatch` provides an alternative implementation.
elif asyncItersBackend == "chronos":
  from chronos/asyncloop import nil

  export asyncloop
