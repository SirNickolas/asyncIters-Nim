import std/macros
from   ./utils import copyLineInfoTo, morphInto

func checkReturnType(params: NimNode): NimNode =
  ## Extract the return type from iterator’s params and validate it’s some kind of `Future[T]`.
  result = params[0]
  if not `or`(
    result.kind == nnkBracketExpr and result.len == 2,
    result.kind in CallNodes and result.len == 3 and result[0].eqIdent"[]",
  ):
    error "async iterator must yield Future[T]", result or params

func transformIterDef(iterDef: NimNode): NimNode =
  ## Turn an `iterator` into an async `proc`.
  let params = iterDef[3]
  if params.len != 1:
    error(
      "parameterized async iterators are currently unsupported." &
      " You can probably achieve what you are trying to by wrapping the iterator in a proc",
      params,
    )
  let
    returnType = params.checkReturnType
    yieldType = returnType[^1]
    bodySym = genSym(nskParam, "body")
    itemSym = ident"item" # For friendlier error messages.
    iterBody = iterDef[6]
  returnType[^1] = bindSym"uint32"

  result = iterDef.morphInto nnkProcDef
  params.add (quote do:
    let `bodySym`: proc (`itemSym`: `yieldType`): `returnType` {.gcsafe.}
  )[0]
  result.addPragma ident"async" # An open symbol to allow custom `async` implementations.
  result[6] = quote:
    template yieldAsync(value: typed) {.used.} =
      if (let ret = await `bodySym` value; ret != 0'u32):
        return ret

    template yieldAsyncFrom(iter: typed) {.used.} =
      if (let ret = await iter `bodySym`; ret != 0'u32):
        return ret

    block:
      `iterBody`

func transformIterList(node: NimNode): NimNode =
  ## Recursively process the statement list containing iterator definitions.
  node.expectKind {nnkIteratorDef, nnkPar, nnkStmtList}
  if node.kind == nnkIteratorDef:
    node.transformIterDef
  else:
    for i, child in node:
      node[i] = child.transformIterList
    node

macro asyncIter*(iterDef: untyped): untyped =
  ##[
    Define an async iterator. It can have `yieldAsync` and `yieldAsyncFrom` statements in its body.

    This macro can be applied to either individual iterator definitions (`{.asyncIter.}`) or entire
    sections of code containing them (`asyncIter:`).
  ]##
  result = iterDef.transformIterList
  when defined asyncIters_debugAsync:
    echo result.repr
