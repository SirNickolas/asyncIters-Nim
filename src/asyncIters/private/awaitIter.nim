import std/macros
from   std/strutils import nimIdentNormalize
import std/tables
from   letUtils import asLet, asVar
from   ./utils import copyLineInfoTo, morphInto

type SomeAsyncIterator[T; F] = proc (body: proc (item: T): F {.gcSafe.}): F {.gcSafe.}

template customAsyncIterator*(T, fut: typed): type =
  ##[
    Type of async iterators after they are processed. `T` is the type of values an iterator yields;
    `fut` is the future type constructor those values are wrapped with. The only requirement
    is that `fut` must be instantiable with one generic parameter (i.e., `fut[U]`).
  ]##
  SomeAsyncIterator[T, fut[uint32]]

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
    failedToReturnLit: bool
    knownNamedBlocks: Table[string, NamedBlock]
    # Fields for `return` support:
    resultSym: NimNode ## `nnkSym` or `nil` if not allocated yet.
    plainReturnMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnLitMagicCode: NimNode ## `nnkUInt32Lit` or `nil` if not allocated yet.
    returnLit: NimNode ## One of `nnkLiterals` or `nnkNone` if can return different values.
    deferredReturnLists: seq[NimNode] ## `seq[nnkStmtList]`
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
    one:  newLit 1'u32,
    magicSym:     bindSym"asyncLoopMagic",
    magicCodeSym: bindSym"asyncLoopMagicCode",
    maxMagicCode: 1, # 0 is reserved for `continue`; 1 is reserved for `break`.
  )

template getOrAllocate(lval, initializer: untyped): untyped =
  ## If `lval` is not nil, return it. Otherwise, evaluate `initializer` and assign to `lval`.
  lval.asVar tmp:
    if tmp.isNil:
      tmp = initializer
      lval = tmp

func createResult(mctx): NimNode =
  mctx.resultSym.getOrAllocate genSym(nskTemplate, "result") # Must be named `result`.

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

proc maybeEnterNamedBlock(mctx; node: NimNode): string =
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
      if pragma.kind in {nnkExprColonExpr} + CallNodes and pragma[0] == mctx.magicSym:
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

proc transformBreakStmt(mctx; brk: NimNode; interceptPlainBreak: bool): NimNode =
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
  if mctx.returnLitMagicCode.isNil:
    # This is the first `return val` statement we've encountered.
    mctx.returnLitMagicCode = nnkUInt32Lit.newNimNode
    mctx.returnLit = val # Remember it so that we can compare subsequent values against it.
    val.kind in nnkLiterals
  else:
    # Check if it is the same literal we've seen the first time.
    val == mctx.returnLit

func processReturnVal(mctx; val, magicStmts: NimNode): NimNode =
  ## Process the value of a `return` statement, adding necessary statements to `magicStmts`,
  ## and return the magic code chosen for this statement.
  if val.kind != nnkEmpty:
    # A `return val` statement.
    if not mctx.failedToReturnLit:
      if mctx.canHandleReturnValLazily val:
        mctx.deferredReturnLists &= magicStmts
        return mctx.returnLitMagicCode

      # This is the first nontrivial `return val` statement we've encountered.
      mctx.failedToReturnLit = true
    # -> result = val
    magicStmts.add mctx.createResult.newAssignment val
  # A plain `return` statement.
  mctx.plainReturnMagicCode.getOrAllocate nnkUInt32Lit.newNimNode

func transformReturnStmt(mctx; ret: NimNode): NimNode =
  result = nnkStmtList.newNimNode
  let val = mctx.processReturnVal(ret[0], magicStmts = result)
  # -> ...; {.asyncLoopMagic: val.}: return asyncLoopMagicCode(val)
  ret[0] = mctx.magicCodeSym.newCall val
  result.add mctx.wrapWithMagic(val, ret)

proc transformBody(mctx; tree: NimNode; interceptBreakContinue: bool): bool =
  ## Recursively traverse the loop body and transform it. Return `true` iff current node should
  ## not be processed further.
  if tree.kind in RoutineNodes - {nnkTemplateDef} or mctx.maybeTransformMagicReturn tree:
    return true
  mctx.withMaybeNamedBlock tree:
    # We should stop intercepting `break` and `continue` when descending into the last child
    # of a nested loop. `block` statements are not treated specially since unlabeled `break`
    # inside a `block` is deprecated and will change its meaning to what we already do now.
    let loopBodyIndex = if tree.kind not_in {nnkForStmt, nnkWhileStmt}: -1 else: tree.len - 1
    for i, node in tree:
      # Recurse.
      if not mctx.transformBody(node, interceptBreakContinue and i != loopBodyIndex):
        # Not a routine definition, a magic return section, nor a named block.
        tree[i] = case node.kind:
          of nnkIdent:
            if not node.eqIdent"result":
              continue
            mctx.createResult
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
    # -> of ...: return
    result.add mctx.assignMagicCode(mctx.plainReturnMagicCode).add do:
      nnkReturnStmt.newTree newEmptyNode()

  if not mctx.returnLitMagicCode.isNil:
    if not mctx.failedToReturnLit:
      # -> of ...: return returnLit
      result.add mctx.assignMagicCode(mctx.returnLitMagicCode).add do:
        nnkReturnStmt.newTree mctx.returnLit
    else:
      # We were hoping to return a literal first but abandoned that idea. All `return` statements
      # we've managed to generate under that optimistic assumption should use the same magic code
      # as statements generated afterwards.
      mctx.returnLitMagicCode.intVal = mctx.plainReturnMagicCode.intVal
      # Patch statements we've deferred.
      let resultSym = mctx.resultSym
      let initiallySeenLit = mctx.returnLit
      for deferred in mctx.deferredReturnLists:
        # -> result = initiallySeenLit
        deferred.insert 0, resultSym.newAssignment initiallySeenLit

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
      # -> of 0'u32, 1'u32: discard
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

func createDeclarations(ctx): NimNode =
  ## Generate declarations that must be visible to the loop body.
  if ctx.resultSym.isNil:
    nnkStmtList.newNimNode
  else:
    let resultSym = ctx.resultSym
    quote:
      # We have to capture the address to avoid the compilation error regarding memory safety.
      let resultAddr = addr result
      # Syntactical presence of `result` in the body does not guarantee it is actually accessed
      # so we need `{.used.}`.
      template `resultSym`: untyped {.used.} = resultAddr[]

proc processBody(body: NimNode): tuple[decls, invoker, invocationWrapper: NimNode] =
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
      discard mctx.transformBody(body, interceptBreakContinue = true)
      (mctx.createCaseDispatcher retVar, mctx) # The order of access to `mctx` is important.
    dispatcher = ctx.patchCaseDispatcher caseStmt
  result.decls = ctx.createDeclarations
  result.invoker =
    if dispatcher.isNil:
      # -> discard ...
      result.invocationWrapper = nnkDiscardStmt.newNimNode
      result.invocationWrapper
    else:
      # -> let ret = ...
      result.invocationWrapper = nnkIdentDefs.newTree(retVar, newEmptyNode())
      newStmtList(
        nnkLetSection.newTree result.invocationWrapper,
        dispatcher,
      )

macro awaitEach(iter: SomeAsyncIterator; originalBody: untyped; loopVars: varargs[untyped]) =
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
  # Rewrite the loop into `awaitEach` call.
  result = loop.copyLineInfoTo bindSym"awaitEach".newCall(invocation[1], loop[^1]) # Iterator, body.
  for i in 0 ..< loop.len - 2: # Loop variables.
    result.add loop[i]
