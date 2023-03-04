##[
  This module implements asynchronous iterators. For more information about asynchronous procedures
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

from   std/asyncdispatch import nil
from   std/asyncfutures import nil
import std/macros

export asyncdispatch.async
when declared asyncdispatch.await:
  export asyncdispatch.await
export asyncfutures except callSoon # `asyncdispatch` provides an alternative implementation.

type
  CustomAsyncIterator*[T; F] = proc (body: proc (item: T): F): F
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `CustomAsyncIterator[T, F]`.
  AsyncIterator*[T] = CustomAsyncIterator[T, asyncfutures.Future[uint32]]
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `AsyncIterator[T]`.

func copyLineInfoTo(info, arg: NimNode): NimNode =
  arg.copyLineInfo info
  arg

func morphInto(prototype: NimNode; kind: NimNodeKind; indices: Slice[int]): NimNode =
  ## Create a new node of type `kind` and add `prototype[indices]` as its children.

  result = kind.newNimNode prototype
  for i in indices:
    result.add prototype[i]

func morphInto(prototype: NimNode; kind: NimNodeKind; start = 0): NimNode =
  ## Create a new node of type `kind` and add `prototype[start ..^ 1]` as its children.

  prototype.morphInto(kind, start ..< prototype.len)

func checkReturnType(params: NimNode): NimNode =
  ## Extract the return type from iterator’s params and validate it’s some kind of `Future[T]`.

  result = params[0]
  if result.kind != nnkBracketExpr or result.len != 2:
    error "async iterator must yield Future[T]", result or params

func desugarYields(iterBody, loopBodySym: NimNode) =
  ## Recursively traverse `iterBody` and transform `yieldAsync` and `yieldAsyncFrom` calls.

  let
    retSym = genSym(ident = "ret")
    await = ident"await"
    zero = newLit 0'u32

  func recurse(node: NimNode) =
    for i, child in node:
      if child.kind not_in RoutineNodes: # Do not descend into nested procedures.
        child.recurse
      if child.kind in nnkCallKinds:
        let callee = child[0]
        let (sink, arg) =
          if callee.eqIdent"yieldAsync":
            if child.len == 1:
              error "need a value to yield", child
            (loopBodySym, # Will invoke loop body with the yielded values.
              if child.len == 2:
                child[1] # A single yielded value.
              else:
                child.morphInto(nnkTupleConstr, 1) # Collect values into a tuple.
            )
          elif callee.eqIdent"yieldAsyncFrom":
            if child.len != 2:
              error "need a single async iterator to yield from", child
            (child[1], loopBodySym) # Will invoke another iterator with the loop body.
          else:
            continue

        node[i] = child.copyLineInfoTo quote do:
          if (let `retSym` = `await` `sink` `arg`; `retSym` != `zero`):
            return `retSym`

  iterBody.recurse

macro asyncIter*(iterDef: untyped): untyped =
  ## Define an async iterator. It can have `yieldAsync` and `yieldAsyncFrom` statements in its body.

  iterDef.expectKind nnkIteratorDef
  let params = iterDef.params
  if params.len != 1:
    error(
      "parameterized async iterators are currently unsupported." &
      " You can probably achieve what you are trying to by wrapping the iterator in a proc",
      params,
    )
  let
    returnType = params.checkReturnType
    yieldType = returnType[1]
    bodySym = genSym(nskParam, "body")
  returnType[1] = bindSym"uint32"

  # Turn the `iterator` into a `proc`.
  result = iterDef.morphInto nnkProcDef
  params.add (quote do:
    let `bodySym`: proc (item: `yieldType`): `returnType`
  )[0]
  result.addPragma ident"async" # An open symbol to allow custom `async` implementations.
  result.body.desugarYields bodySym

  when defined asyncIters_debugAsync:
    echo result.repr

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
