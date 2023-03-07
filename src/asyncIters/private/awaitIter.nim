from   std/asyncfutures import nil
import std/macros
from   std/strutils import nimIdentNormalize
import std/tables
from   letUtils import asLet, asVar
from   ./utils import copyLineInfoTo, morphInto

type
  CustomAsyncIterator*[T; F] = proc (body: proc (item: T): F): F
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `CustomAsyncIterator[T, F]`.
  AsyncIterator*[T] = CustomAsyncIterator[T, asyncfutures.Future[uint32]]
    ## Type of async iterators after they are processed. Do not make any assumptions about
    ## its definition — it is an implementation detail. Just use `AsyncIterator[T]`.

func safeSignature(node: NimNode): string =
  ## Return a string that uniquely identifies the node (which must be either `nnkIdent`
  ## or `nnkSym`).

  if node.kind == nnkIdent:
    node.strVal.nimIdentNormalize.asVar: &= '.' # Period prevents clashing with symbols.
  else:
    node.expectKind {nnkIdent, nnkSym} # For better error messages.
    node.signatureHash

func prepareLoopVarAndBody(loopVars, body: NimNode): (NimNode, NimNode) =
  ## Extract loop variable from an `Arglist`. If there are several variables, create a tuple
  ## parameter and generate code that unpacks it.

  let tupleParam = genSym(nskParam, "item")
  let section = nnkLetSection.newNimNode
  if loopVars.len == 1:
    let loopVar = loopVars[0]
    if loopVar.kind != nnkVarTuple: # A single loop variable.
      return (loopVar, body)
    section.add loopVar.add tupleParam # A single destructuring statement.
  else:
    # Multiple loop variables (need to unpack a tuple). Nim does not currently support recursive
    # tuple destructuring so we cannot just morph `loopVars` into `nnkVarTuple`.
    let rootVarTuple = nnkVarTuple.newNimNode
    section.add rootVarTuple
    for loopVar in loopVars:
      rootVarTuple.add:
        if loopVar.kind != nnkVarTuple:
          loopVar
        else:
          genSym(ident = "tuple").asLet aux:
            section.add loopVar.add aux
    rootVarTuple.add newEmptyNode(), tupleParam
  (tupleParam, newStmtList(section, body))

type
  NamedBlock = object
    nestedDefs: int ## How many times blocks with this name have been declared on the lexical stack.
    breakStmt: NimNode ## `nnkBreakStmt` or `nil` if not allocated yet.
    magicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.

  Context = object
    zero, one, magicSym, magicCodeSym: NimNode ## Immutable fields.
    maxMagicCode: uint32
    hasPlainBreak: bool
    knownNamedBlocks: Table[string, NamedBlock]
    # Fields for `return` support:
    plainReturnMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnValMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnLit: NimNode ## One of `nnkLiterals` or `nil` if can return different values.
    deferredReturnLists: seq[NimNode] ## `seq[nnkStmtList]`
    resultVar: NimNode ## `nnkSym` or `nil` if not allocated yet.
    forwardedReturnStmt: NimNode ## Typically, `nnkStmtList`; or `nil` if not occurred.

using
  ctx: Context
  mctx: var Context

template asyncLoopMagic(code: uint32; body: untyped): untyped = body
  ## A no-op transformer used to mark `return` statements that have already been processed.

template asyncLoopMagicCode(code: uint32): uint32 = code
  ## A no-op transformer used to mark a value that is being returned.

func initContext: Context =
  Context(
    zero: newLit 0'u32,
    one: newLit 1'u32,
    magicSym: bindSym"asyncLoopMagic",
    magicCodeSym: bindSym"asyncLoopMagicCode",
    maxMagicCode: 1, # 0 is reserved for `continue`; 1 is reserved for `break`.
  )

template getOrAllocate(lval, initializer: untyped): untyped =
  ## If `lval` is not nil, return it. Otherwise, evaluate `initializer` and assign to `lval`.

  lval.asVar tmp:
    if tmp.isNil:
      tmp = initializer
      lval = tmp

func newBareMagicReturn(ctx; val, prototype: NimNode): NimNode =
  # -> return asyncLoopMagicCode(val)
  nnkReturnStmt.newNimNode(prototype).add(ctx.magicCodeSym.newCall val)

func newMagicReturn(ctx; val, prototype: NimNode): NimNode =
  # -> asyncLoopMagic(val): return asyncLoopMagicCode(val)
  ctx.magicSym.newCall(val, ctx.newBareMagicReturn(val, prototype))

func maybeEnterNamedBlock(mctx; node: NimNode): string =
  ## If `node` is a named `block` statement or expression, remember it in the context and return
  ## the signature of its name. Otherwise, do nothing and return an empty string.

  if node.kind in {nnkBlockStmt, nnkBlockExpr}:
    let name = node[0]
    if name.kind != nnkEmpty:
      return name.safeSignature.asLet signature:
        mctx.knownNamedBlocks.mgetOrPut(signature, NamedBlock()).nestedDefs += 1

func maybeLeaveNamedBlock(mctx; signature: string): bool {.discardable.} =
  ## Unless `signature` is an empty string, unregister the named block it refers to from the context
  ## and return `true`. Otherwise, return `false`.

  result = signature.len != 0
  if result:
    mctx.knownNamedBlocks[signature].nestedDefs -= 1

template withMaybeNamedBlock(mctx: Context; node: NimNode; body: untyped): bool =
  ## Evaluate `body` with proper bookkeeping. Return `true` iff `node` is a named block.

  let signature = mctx.maybeEnterNamedBlock node
  body
  mctx.maybeLeaveNamedBlock signature

func maybeTransformMagicReturn(mctx; node: NimNode): bool =
  ## If `node` is an `asyncLoopMagic(...)` call, process it and return `true`. Otherwise, return
  ## `false`.

  result = node.kind in CallNodes and node[0] == mctx.magicSym
  if result:
    # This is our `asyncLoopMagic` annotation put by an outer `awaitIter` (we are a nested loop).
    if mctx.forwardedReturnStmt.isNil:
      let x = node[1].intVal.uint32
      if x > mctx.maxMagicCode:
        mctx.maxMagicCode = x
      # We assume that `async` transforms `return` statements uniformly (i.e., doesn't
      # special-case anything). Therefore, we can remember only one of its transformation
      # results and expect all others to look similarly.
      mctx.forwardedReturnStmt = node[2]
    # -> return asyncLoopMagicCode(...'u32)
    node[2] = mctx.newBareMagicReturn(node[1], prototype = node[2])

func transformBreakStmt(mctx; brk: NimNode; interceptPlainBreak: bool): NimNode =
  let blockName = brk[0]
  if blockName.kind != nnkEmpty:
    # A labeled `break`.
    let blk = addr mctx.knownNamedBlocks.mgetOrPut(blockName.safeSignature, NamedBlock())
    if blk.nestedDefs == 0:
      # This block is declared outside the loop body.
      if blk.magicCode.isNil:
        # This is the first time we break out of this block.
        blk.breakStmt = brk
        blk.magicCode = nnkUInt32Lit.newNimNode
      # -> asyncLoopMagic(...'u32): return asyncLoopMagicCode(...'u32)
      return mctx.newMagicReturn(blk.magicCode, prototype = brk)
  elif interceptPlainBreak:
    # An unlabeled `break`.
    mctx.hasPlainBreak = true
    # -> asyncLoopMagic(1'u32): return asyncLoopMagicCode(1'u32)
    return mctx.newMagicReturn(mctx.one, prototype = brk)
  brk

func canHandleReturnValLazily(mctx; val: NimNode): bool =
  ## Return `true` if `val` equals every other return value seen before.

  if mctx.returnValMagicCode.isNil:
    # This is the first `return val` statement we've encountered.
    mctx.returnValMagicCode = nnkUInt32Lit.newNimNode
    # If `val` is a literal, remember it. Otherwise, remember a node that will not be equal
    # to anything (unless someone maliciously constructs an AST with `nnkNone`...). We cannot use
    # `nil` for that purpose since `==` is buggy: `newNilNode() == nil` is true.
    (val.kind in nnkLiterals).asLet isSomeLit:
      mctx.returnLit = if isSomeLit: val else: nnkNone.newNimNode
  else:
    # Check if it is the same literal we've seen the first time.
    val == mctx.returnLit

func processReturnVal(mctx; val, magicStmts: NimNode): NimNode =
  ## Process the value of a `return` statement, adding necessary statements to `magicStmts`,
  ## and return the magic code chosen for this statement.

  if val.kind == nnkEmpty:
    # A plain `return` statement.
    mctx.plainReturnMagicCode.getOrAllocate nnkUInt32Lit.newNimNode
  else:
    # A `return val` statement.
    if mctx.canHandleReturnValLazily val:
      mctx.deferredReturnLists &= magicStmts
    else:
      let resultVar = mctx.resultVar.getOrAllocate:
        # This is the first nontrivial `return val` statement we've encountered.
        genSym(nskVar, "result").asLet resultVar:
          let seenLit = mctx.returnLit
          mctx.returnLit = nnkNone.newNimNode # Forget it. From now on, we will always be eager.
          # Patch statement lists we've deferred.
          for deferred in mctx.deferredReturnLists:
            # -> resultVar = seenLit
            deferred.insert 0, resultVar.newAssignment seenLit
      # -> resultVar = val
      magicStmts.add resultVar.newAssignment val
    mctx.returnValMagicCode

func transformReturnStmt(mctx; ret: NimNode): NimNode =
  let stmts = nnkStmtList.newNimNode
  let val = mctx.processReturnVal(ret[0], magicStmts = stmts)
  # -> asyncLoopMagic(val): return asyncLoopMagicCode(val)
  ret[0] = mctx.magicCodeSym.newCall val
  mctx.magicSym.newCall(val, stmts.add ret)

func transformBody(mctx; tree: NimNode; interceptBreakContinue: bool) =
  # We should stop intercepting `break` and `continue` when descending into the last child
  # of a nested loop. `block` statements are not treated specially since unlabeled `break`
  # inside a `block` is deprecated and will change its meaning to what we already do now.
  let loopBodyIndex = if tree.kind not_in {nnkForStmt, nnkWhileStmt}: -1 else: tree.len - 1
  for i, node in tree:
    if not mctx.maybeTransformMagicReturn(node) and not (block:
      # Recurse.
      mctx.withMaybeNamedBlock node:
        mctx.transformBody(node, interceptBreakContinue and i != loopBodyIndex)
    ): # Not a magic return section nor a named block.
      tree[i] = case node.kind:
        of nnkBreakStmt:
          mctx.transformBreakStmt(node, interceptBreakContinue)
        of nnkContinueStmt:
          if not interceptBreakContinue:
            continue
          # -> asyncLoopMagic(0'u32): return asyncLoopMagicCode(0'u32)
          mctx.newMagicReturn(mctx.zero, prototype = node)
        of nnkReturnStmt:
          mctx.transformReturnStmt node
        else:
          continue

func assignMagicCode(mctx; test: NimNode): NimNode =
  ## Allocate a new `uint32` value and assign it to `test`, which must be an `nnkUInt32Lit`.
  ## Return an `nnkOfBranch` with `test` as its first child.

  let code = mctx.maxMagicCode + 1
  mctx.maxMagicCode = code
  test.intVal = code.BiggestInt
  nnkOfBranch.newTree test

func replaceMagicCode(ctx; tree, repl: NimNode): NimNode =
  ## Traverse `tree` and replace `asyncLoopMagicCode(...)` with `repl`.

  let magic = ctx.magicCodeSym
  if tree.kind in CallNodes and tree[0] == magic:
    repl
  else:
    func recurse(tree: NimNode) =
      for i, child in tree:
        if child.kind in CallNodes and child[0] == magic:
          tree[i] = repl
        else:
          child.recurse

    tree.recurse
    tree

func createCaseDispatcher(mctx; retVar: NimNode): NimNode =
  ## Create an **incomplete** `case` statement that handles the magic code returned from the loop
  ## body (expected to be stored in `retVar`).

  # -> case ...
  result = nnkCaseStmt.newTree(
    nnkStmtListExpr.newNimNode,
    nnkOfBranch.newNimNode, # A branch for `0, 1`.
  )
  if not mctx.plainReturnMagicCode.isNil:
    # -> of ...: return
    result.add:
      mctx.assignMagicCode(mctx.plainReturnMagicCode).add(nnkReturnStmt.newTree newEmptyNode())
  if not mctx.returnValMagicCode.isNil:
    # -> of ...: return resultVar
    result.add:
      mctx.assignMagicCode(mctx.returnValMagicCode).add:
        nnkReturnStmt.newTree if mctx.returnLit.isNil: mctx.resultVar else: mctx.returnLit
  for blk in mctx.knownNamedBlocks.values:
    if not blk.magicCode.isNil:
      # -> of ...: break ...
      result.add mctx.assignMagicCode(blk.magicCode).add(blk.breakStmt)
  if not mctx.forwardedReturnStmt.isNil:
    # -> else: ...
    result.add nnkElse.newTree mctx.replaceMagicCode(mctx.forwardedReturnStmt, repl = retVar)

func completeCaseDispatcher(ctx; caseStmt, retVar: NimNode): NimNode =
  ## Patch the `case` statement made by `createCaseDispatcher` and return the node where `retVar`’s
  ## initializer should be added.

  let empty = newEmptyNode()
  # -> case (...)
  let caseExpr = caseStmt[0]
  if not ctx.resultVar.isNil:
    # -> (var resultVar: typeOf(result); ...)
    caseExpr.add nnkVarSection.newTree nnkIdentDefs.newTree(
      ctx.resultVar,
      bindSym"typeOf".newCall ident"result",
      empty,
    )
  # -> (...; let ret = ...; ret)
  result = nnkIdentDefs.newTree(retVar, empty)
  caseExpr.add nnkLetSection.newTree result, retVar

  # -> of 0, 1: discard
  let firstBranch = caseStmt[1]
  firstBranch.add ctx.zero
  if ctx.hasPlainBreak:
    firstBranch.add ctx.one
  firstBranch.add nnkDiscardStmt.newTree empty

  # If the last branch of a `case` is `of`, turn it into `else`.
  let lastBranch = caseStmt[^1]
  if lastBranch.kind == nnkOfBranch:
    caseStmt[^1] = nnkElse.newTree lastBranch[1]

func createDispatcher(body: NimNode): (NimNode, NimNode) =
  ##[
    Transform loop body and generate code that runs it. Return a `(dispatcher, invocationWrapper)`
    pair: `dispatcher` is the code that runs the loop and does some postprocessing and
    `invocationWrapper` is the node you should add body invocation to.
  ]##
  var mctx = initContext()
  body.expectKind nnkStmtList
  mctx.transformBody(body, interceptBreakContinue = true)
  let retVar = genSym(ident = "ret")
  let caseStmt = mctx.createCaseDispatcher retVar
  case caseStmt.len:
    of 2:
      # -> discard ...
      let d = nnkDiscardStmt.newNimNode
      (d, d)
    of 3:
      # There is only one nontrivial branch. Can emit `if` instead of `case`.
      let cond = nnkInfix.newNimNode
      if mctx.hasPlainBreak:
        # -> 1'u32 < ...
        cond.add bindSym"<", mctx.one
      else:
        # -> 0'u32 != ...
        cond.add bindSym"!=", mctx.zero # `test, jnz` is better than `cmp, ja`.
      # -> if ...: ...
      (newIfStmt (cond, caseStmt[2][^1]), cond)
    else:
      (caseStmt, mctx.completeCaseDispatcher(caseStmt, retVar))

macro awaitEach(iter: CustomAsyncIterator; originalBody: untyped; loopVars: varargs[untyped]) =
  ## Transform the loop body into an asynchronous procedure and run it.

  let
    (futureType, yieldType) = block:
      let params = iter.getTypeImpl[0]
      (params[0], params[1][1][0][1][1])
    (loopParam, body) = prepareLoopVarAndBody(loopVars, originalBody)
    (dispatcher, invocationWrapper) = originalBody.createDispatcher
    bodyProcSym = genSym(nskProc, "asyncForBody") # For better stack traces.
  result = newStmtList(
    newProc(
      name = bodyProcSym,
      params = [futureType, newIdentDefs(loopParam, yieldType)],
      pragmas = nnkPragma.newTree ident"async",
      body = body,
    ),
    dispatcher,
  )
  # -> ... await(iter(asyncForBody))
  invocationWrapper.add iter.copyLineInfoTo(ident"await").newCall(iter.newCall bodyProcSym)

  when defined asyncIters_debugAwait:
    echo result.repr

macro awaitEach(iter: typed; loopBodyAndVars: varargs[untyped]) =
  ## An overload that emits a helpful error message when `iter` has incorrect type.

  error "awaitIter expects an async iterator, got " & iter.getTypeInst.repr, iter

macro awaitIter*(loop: ForLoopStmt) =
  ## Iterate over an async iterator. Like regular `await`, this can only occur in procedures
  ## marked with `{.async.}` or `{.asyncIter.}`.

  let invocation = loop[^2] # `awaitIter(...)`
  invocation.expectLen 2
  # Transform the loop into `awaitEach` call.
  result = loop.copyLineInfoTo bindSym"awaitEach".newCall(invocation[1], loop[^1]) # Iterator, body.
  for i in 0 ..< loop.len - 2: # Loop variables.
    result.add loop[i]
