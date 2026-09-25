## stack-safety-rest - stages 7 and 8 of the stack-safety audit: three holes in the small modules, and the rest bounded

- okay2's `Cst.rebase` walked a parsed tree with a native frame per
  level; the Scala 3 twin had been an explicit stack since
  cst-walk-stack-safe. Ported: post-order on a stack of open nodes,
  20 000 levels in okay2's TestParse (red first).
- okay2's `retry` called itself inside the `catch` once per retry — a
  call Scala 2 cannot turn into a jump (the Scala 3 twin's `@tailrec`
  is), so a policy that kept retrying overflowed at 200 000. A loop now
  (TestParallel, red first).
- `Classify.example` unfolded a recursive intent type for ever: a
  derived schema of a recursive type is a cycle. A product or sum met
  again on the path is an empty object now (TestClassifyDepth, red
  first).

The other 110 rows of stages 7 and 8 carry their bounds in the
inventory: every macro walk runs at compile time over the user's
source; the row-type derivations walk a type's width; the actor tree,
a Flow, a route trie keyed by segment count, a source tree on disk
(PATH_MAX), a Cardano datum (maxTxSize), the developer's script modules
(a cycle refused) are each the program's or the platform's own bound;
and the callback loops — the Async runner's re-entry, core.async's
dispatcher, cats-effect and Kyo binds, pool/bulkhead/hedge completions,
the LLM stream's program binds — run later from another frame.
With this the staged audit of specs/stack-safety.md has no open stage
left; what remains open is named there (Decision 7's Proc composition,
1c's Cont spec).
