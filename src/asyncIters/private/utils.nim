## Utility functions for working with Nim AST.

import std/macros

func copyLineInfoTo*(info, arg: NimNode): NimNode =
  arg.copyLineInfo info
  arg

func morphInto*(prototype: NimNode; kind: NimNodeKind; indices: Slice[int]): NimNode =
  ## Create a new node of type `kind` and add `prototype[indices]` as its children.
  result = kind.newNimNode prototype
  for i in indices:
    result.add prototype[i]

func morphInto*(prototype: NimNode; kind: NimNodeKind; start = 0): NimNode =
  ## Create a new node of type `kind` and add `prototype[start ..^ 1]` as its children.
  prototype.morphInto(kind, start ..< prototype.len)
