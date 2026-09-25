## stack-safety-query - a Query predicate built by a fold renders, lists and runs at any depth, in both cores

Stack-safety stage 4's named suspect, confirmed. A `Query.Where` built the
natural way, `conds.reduce(_ and _)`, is a tree as deep as the list is
long. `render` (with `both`), `collect` and `eval` recursed once per
`and`/`or`/`not`, and 200 000 conditions overflowed in okay-sql and
okay2-sql alike (`TestQueryDepth`, red first in both, all platforms).

- `render` now writes left to right into one `StringBuilder`, over an
  explicit task stack. A post-order pass on a worklist first marks the
  subtrees that render as nothing (`True`, and an and/or of two such), so
  the empty clause drops out exactly as before. The new test pins every
  such case, `NOT ()` included. The old render copied the whole string
  built below each level, which was quadratic in depth; that copy is gone.
- `collect` is a worklist.
- `eval` is a small continuation machine (negate / and-then / or-then)
  that keeps `&&`/`||` short-circuiting: the right side of an and is not
  run once the left is false.

The eight inventory rows are deleted, as the stage-9 guard required
(`recscan-check.sh --write`). okay-py's `ArrowFrames.cells` is marked
BOUNDED: it walks a Table's columns, and Table refuses types past 64 since
stack-safety-arrow. The backlog items keep what is left: `ContMacro`, and
okay2's `Typed.fits`, jdbc, spark and the fs2/zio interop.
