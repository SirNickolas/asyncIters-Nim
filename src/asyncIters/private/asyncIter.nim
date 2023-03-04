import std/macros
from   ./utils import copyLineInfoTo, morphInto

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
      if child.kind in CallNodes:
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
