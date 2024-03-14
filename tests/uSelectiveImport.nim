# https://github.com/SirNickolas/asyncIters-Nim/issues/1
from asyncIters import AsyncIterator

{.used.}

type A {.used.} = AsyncIterator[int]
