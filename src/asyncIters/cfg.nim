from std/strutils import normalize

const
  asyncBackend {.strdefine.} = "asyncdispatch"
  backend* = asyncBackend.normalize
    ## The value of `-d=asyncBackend:...`:option: compile-time switch, processed
    ## by `strutils.normalize <https://nim-lang.org/docs/strutils.html#normalize,string>`_.
