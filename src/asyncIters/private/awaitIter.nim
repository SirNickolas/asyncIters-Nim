from   std/asyncfutures import nil
import std/macros
from   ./utils import copyLineInfoTo, morphInto

type
  CustomAsyncIterator*[T; F] = proc (body: proc (item: T): F): F
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `CustomAsyncIterator[T, F]`.
  AsyncIterator*[T] = CustomAsyncIterator[T, asyncfutures.Future[uint32]]
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `AsyncIterator[T]`.

template asyncLoopMagic(body: untyped): untyped = body
  ## A no-op transformer used to mark `return` statements that have already been processed.

func prepareLoopVarAndBody(loopVarsAndBody: NimNode): (NimNode, NimNode, NimNode) =
  ## Extract loop variable and body from the `Arglist`. If there are several variables, create
  ## a tuple parameter and generate code that unpacks it.

  let
    tupleParam = genSym(nskParam, "item")
    section = nnkLetSection.newNimNode
    body = loopVarsAndBody[^1]
  if loopVarsAndBody.len == 2:
    let loopVar = loopVarsAndBody[0]
    if loopVar.kind != nnkVarTuple: # A single loop variable.
      return (loopVar, body, body)
    section.add loopVar.add tupleParam # A single destructuring statement.
  else:
    # Multiple loop variables (need to unpack a tuple). Nim does not currently support recursive
    # tuple destructuring so we cannot just morph `loopVarsAndBody` into `nnkVarTuple`.
    let rootVarTuple = nnkVarTuple.newNimNode
    section.add rootVarTuple
    for i in 0 ..< loopVarsAndBody.len - 1:
      let loopVar = loopVarsAndBody[i]
      rootVarTuple.add:
        if loopVar.kind != nnkVarTuple:
          loopVar
        else:
          let aux = genSym(ident = "tuple")
          section.add loopVar.add aux
          aux
    rootVarTuple.add newEmptyNode(), tupleParam
  (tupleParam, newStmtList(section, body), body)

macro awaitEach(iter: CustomAsyncIterator; loopVarsAndBody: varargs[untyped]) =
  ## Iterate over an async iterator. Like regular `await`, this can only occur in procedures
  ## marked with `{.async.}` or `{.asyncIter.}`.

  let
    (futureType, yieldType) = block:
      let params = iter.getTypeImpl[0]
      (params[0], params[1][1][0][1][1])
    (loopVar, body, originalBody) = loopVarsAndBody.prepareLoopVarAndBody
    bodyProcSym = genSym(nskProc, "asyncForBody") # For better stack traces.
  result = newStmtList(
    newProc(
      name = bodyProcSym,
      params = [futureType, newIdentDefs(loopVar, yieldType)],
      pragmas = nnkPragma.newNimNode.add ident"async",
      body = body,
    ),
    # `discard await iter asyncForBody`
    nnkDiscardStmt.newNimNode.add nnkCommand.newTree(
      iter.copyLineInfoTo ident"await",
      nnkCommand.newTree(iter, bodyProcSym),
    ),
  )
  # TODO: Process `originalBody`.

  when defined asyncIters_debugAwait:
    echo result.repr

macro awaitEach(iter: typed; loopVarsAndBody: varargs[untyped]) =
  ## An overload that emits a helpful error message when `iter` has incorrect type.

  error "awaitIter expects an async iterator, got " & iter.getTypeInst.repr, iter

macro awaitIter*(loop: ForLoopStmt) =
  ## Iterate over an async iterator. Like regular `await`, this can only occur in procedures
  ## marked with `{.async.}` or `{.asyncIter.}`.

  let invocation = loop[^2] # awaitIter(...)
  invocation.expectLen 2
  # Transform the loop into `awaitEach` call.
  result = nnkCommand.newNimNode(loop).add(bindSym"awaitEach", invocation[1])
  for i in 0 ..< loop.len - 2: # Loop variables.
    result.add loop[i]
  result.add loop[^1] # Body.
