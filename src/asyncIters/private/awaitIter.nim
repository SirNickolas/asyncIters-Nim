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
    resultVar: NimNode ## `nnkSym` or `nil` if not allocated yet.
    # Fields for `return` support:
    plainReturnMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnValMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnLit: NimNode ## One of `nnkLiterals` or `nnkNone` if can return different values.
    deferredReturnLists: seq[NimNode] ## `seq[nnkStmtList]`
    actualResultVar: NimNode ## `nnkSym` or `nil` if not allocated yet.
    forwardedReturnStmt: NimNode ## Typically, `nnkStmtList`; or `nil` if not occurred.

using
  ctx: Context
  mctx: var Context

template asyncLoopMagic(code: uint32) {.pragma.}
  ## A pragma used to mark `return` statements that have already been processed.

template asyncLoopMagicCode(code: uint32): uint32 = code
  ## An identity template used to mark a value that is being returned.

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

func wrapWithMagic(ctx; val, stmts: NimNode): NimNode =
  # -> {.asyncLoopMagic: val.}: stmts
  nnkPragmaBlock.newTree(
    nnkPragma.newTree nnkExprColonExpr.newTree(ctx.magicSym, val),
    stmts,
  )

func newMagicReturn(ctx; val, prototype: NimNode): NimNode =
  # -> {.asyncLoopMagic: val.}: return asyncLoopMagicCode(val)
  ctx.wrapWithMagic(val, ctx.newBareMagicReturn(val, prototype))

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
  ## If `node` is an `{.asyncLoopMagic: ...'u32.}: ...` pragma block, process it and return `true`.
  ## Otherwise, return `false`.

  if node.kind == nnkPragmaBlock:
    for pragma in node[0]:
      if pragma.kind in {nnkExprColonExpr, nnkCall} and pragma[0] == mctx.magicSym:
        # We've found our `asyncLoopMagic` pragma in the loop body passed to us. That means we are
        # a nested loop - the pragma was put by an outer `awaitIter` invocation.
        if mctx.forwardedReturnStmt.isNil:
          let x = pragma[1].intVal.uint32
          if x > mctx.maxMagicCode:
            mctx.maxMagicCode = x
          # We assume that `async` transforms `return` statements uniformly (i.e., doesn't
          # special-case anything). Therefore, we can remember only one of its transformation
          # results and expect all others to look similarly.
          mctx.forwardedReturnStmt = node[1]
        # -> return asyncLoopMagicCode(...'u32)
        node[1] = mctx.newBareMagicReturn(pragma[1], prototype = node[1])
        return true

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
      # -> {.asyncLoopMagic: ...'u32.}: return asyncLoopMagicCode(...'u32)
      return mctx.newMagicReturn(blk.magicCode, prototype = brk)
  elif interceptPlainBreak:
    # An unlabeled `break`.
    mctx.hasPlainBreak = true
    # -> {.asyncLoopMagic: 1'u32.}: return asyncLoopMagicCode(1'u32)
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
      let actualResultVar = mctx.actualResultVar.getOrAllocate:
        # This is the first nontrivial `return val` statement we've encountered.
        genSym(nskVar, "actualResult").asLet actualResultVar:
          let seenLit = mctx.returnLit
          mctx.returnLit = nnkNone.newNimNode # Forget it. From now on, we will always be eager.
          # Patch statement lists we've deferred.
          for deferred in mctx.deferredReturnLists:
            # -> actualResultVar = seenLit
            deferred.insert 0, actualResultVar.newAssignment seenLit
      # -> actualResultVar = val
      magicStmts.add actualResultVar.newAssignment val
    mctx.returnValMagicCode

func transformReturnStmt(mctx; ret: NimNode): NimNode =
  let stmts = nnkStmtList.newNimNode
  let val = mctx.processReturnVal(ret[0], magicStmts = stmts)
  # -> {.asyncLoopMagic: val.}: ...; return asyncLoopMagicCode(val)
  ret[0] = mctx.magicCodeSym.newCall val
  mctx.wrapWithMagic(val, stmts.add ret)

func transformBody(mctx; tree: NimNode; interceptBreakContinue: bool): bool {.discardable.} =
  ## Recursively traverse `tree` and transform it. Return `true` iff `tree` is a named block.

  mctx.withMaybeNamedBlock tree:
    if tree.kind == nnkDotExpr:
      # If `tree` is `x.y`, we should not descend into `y`.
      mctx.transformBody tree[0], interceptBreakContinue
    else:
      # We should stop intercepting `break` and `continue` when descending into the last child
      # of a nested loop. `block` statements are not treated specially since unlabeled `break`
      # inside a `block` is deprecated and will change its meaning to what we already do now.
      let loopBodyIndex = if tree.kind not_in {nnkForStmt, nnkWhileStmt}: -1 else: tree.len - 1
      for i, node in tree:
        if (
          node.kind not_in RoutineNodes and
          not mctx.maybeTransformMagicReturn(node) and
          # Recurse.
          not mctx.transformBody(node, interceptBreakContinue and i != loopBodyIndex)
        ): # Not a routine definition, a magic return section, nor a named block.
          tree[i] = case node.kind:
            of nnkIdent:
              if not node.eqIdent"result":
                continue
              mctx.resultVar.getOrAllocate genSym(nskVar, "result")
            of nnkBreakStmt:
              mctx.transformBreakStmt(node, interceptBreakContinue)
            of nnkContinueStmt:
              if not interceptBreakContinue:
                continue
              # -> {.asyncLoopMagic: 0'u32.}: return asyncLoopMagicCode(0'u32)
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

  # -> case ret
  result = nnkCaseStmt.newTree(retVar, nnkOfBranch.newNimNode) # A branch for `0, 1`.
  if not mctx.plainReturnMagicCode.isNil:
    # -> of ...: return resultVar
    result.add:
      mctx.assignMagicCode(mctx.plainReturnMagicCode).add:
        nnkReturnStmt.newTree if mctx.resultVar.isNil: newEmptyNode() else: mctx.resultVar
  if not mctx.returnValMagicCode.isNil:
    # -> of ...: return actualResultVar
    result.add:
      mctx.assignMagicCode(mctx.returnValMagicCode).add:
        nnkReturnStmt.newTree:
          if mctx.returnLit.kind != nnkNone: mctx.returnLit else: mctx.actualResultVar
  for blk in mctx.knownNamedBlocks.values:
    if not blk.magicCode.isNil:
      # -> of ...: break ...
      result.add mctx.assignMagicCode(blk.magicCode).add(blk.breakStmt)
  if not mctx.forwardedReturnStmt.isNil:
    # -> else: ...
    result.add nnkElse.newTree mctx.replaceMagicCode(mctx.forwardedReturnStmt, repl = retVar)

func patchCaseDispatcher(ctx; caseStmt: NimNode): NimNode =
  ## Transform the `case` statement made by `createCaseDispatcher` into the most appropriate form.
  ## May return `nil` if a dispatcher is not needed at all.

  case caseStmt.len:
    of 2:
      nil # Can ignore the return code.
    of 3:
      # There is only one nontrivial branch. Can emit `if` instead of `case`.
      let cond = nnkInfix.newNimNode
      if ctx.hasPlainBreak:
        # -> 1'u32 < ...
        cond.add bindSym"<", ctx.one
      else:
        # -> 0'u32 != ...
        cond.add bindSym"!=", ctx.zero # `test, jnz` is better than `cmp, ja`.
      cond.add caseStmt[0] # `retVar`
      # -> if ...: ...
      newIfStmt (cond, caseStmt[2][^1])
    else:
      # -> of 0, 1: discard
      let firstBranch = caseStmt[1]
      firstBranch.add ctx.zero
      if ctx.hasPlainBreak:
        firstBranch.add ctx.one
      firstBranch.add nnkDiscardStmt.newTree newEmptyNode()

      # If the last branch of a `case` is `of`, turn it into `else`.
      let lastBranch = caseStmt[^1]
      if lastBranch.kind == nnkOfBranch:
        caseStmt[^1] = nnkElse.newTree lastBranch[1]

      caseStmt

func createAuxilaryVars(ctx): NimNode =
  #[ ->
    var
      resultVar = move(result)
      actualResultVar: typeOf(result)
  ]#
  let empty = newEmptyNode()
  let realResult = ident"result"
  result = nnkVarSection.newNimNode
  if not ctx.resultVar.isNil:
    result.add nnkIdentDefs.newTree(
      ctx.resultVar,
      empty,
      bindSym"move".newCall realResult,
    )
  if not ctx.actualResultVar.isNil:
    result.add nnkIdentDefs.newTree(
      ctx.actualResultVar,
      bindSym"typeOf".newCall realResult, # Might be incorrect if `result` is shadowed.
      empty,
    )

func processBody(body: NimNode): tuple[decls, invoker, invocationWrapper: NimNode] =
  ##[
    Transform the loop body and generate code that runs it. Return a tuple:
    #. `decls` (`nnkStmtList`) are declarations that must be injected prior to the body definition;
    #. `invoker` (`nnkStmtList`) is the code that runs the loop and does some postprocessing;
    #. `invocationWrapper` (a descendant of `invoker`) is where the body invocation should be added.
  ]##
  let
    retVar = genSym(ident = "ret")
    (caseStmt, ctx) = block:
      var mctx = initContext()
      body.expectKind nnkStmtList
      mctx.transformBody(body, interceptBreakContinue = true)
      (mctx.createCaseDispatcher retVar, mctx)

  result.decls = nnkStmtList.newNimNode
  if (let vars = ctx.createAuxilaryVars; vars.len != 0):
    result.decls.add vars

  let dispatcher = ctx.patchCaseDispatcher caseStmt
  result.invoker = newStmtList:
    if dispatcher.isNil:
      # -> discard ...
      result.invocationWrapper = nnkDiscardStmt.newNimNode
      result.invocationWrapper
    else:
      # -> let ret = ...
      result.invocationWrapper = nnkIdentDefs.newTree(retVar, newEmptyNode())
      nnkLetSection.newTree result.invocationWrapper
  if not ctx.resultVar.isNil:
    # -> result = move(resultVar)
    result.invoker.add ident"result".newAssignment bindSym"move".newCall ctx.resultVar
  if not dispatcher.isNil:
    result.invoker.add dispatcher

macro awaitEach(iter: CustomAsyncIterator; originalBody: untyped; loopVars: varargs[untyped]) =
  ## Transform the loop body into an asynchronous procedure and run it.

  let
    (futureType, yieldType) = block:
      let params = iter.getTypeImpl[0]
      (params[0], params[1][1][0][1][1])
    (loopParam, body) = prepareLoopVarAndBody(loopVars, originalBody)
    generated = originalBody.processBody
    bodyProcSym = genSym(nskProc, "asyncForBody") # For better stack traces.
  result = generated.decls
  result.add(
    newProc(
      name = bodyProcSym,
      params = [futureType, newIdentDefs(loopParam, yieldType)],
      pragmas = nnkPragma.newTree ident"async",
      body = body,
    ),
    generated.invoker,
  )
  # -> ... await(iter(asyncForBody))
  generated.invocationWrapper.add:
    iter.copyLineInfoTo(ident"await").newCall(iter.newCall bodyProcSym)

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
