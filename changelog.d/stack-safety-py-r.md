## stack-safety-py-r - the Python and R codecs take any depth; the value walks of okay-py share one explicit-stack walk

Stack-safety stage 4b, and the close of stage 4. Every value walk in
okay-py and okay-r was a hole, and every one was red first at 200 000
levels:

- `PyCodec.enc`/`dec` and `RCodec.enc`/`dec` recursed once per level of
  a value of a recursive type. They take Json's own road now: a direct
  call below `Codecs.NativeThreshold`, the `Cont` trampoline past it
  (`encC`/`decC`), a `Held[Y]` pair carrying a field's schema and value
  through `eachField` without a cast (`TestPyCodecDepth`,
  `TestRCodecDepth`).
- On that road two more defects surfaced. The decode PATH was a string
  grown per level (`"$at.$name"`), quadratic in the depth: a 200 000-deep
  value ran out of heap before it ran out of stack. It is a linked `At`
  now, rendered only into a message, and a refusal at depth still names
  its path. And a refusal printed the value it met with `toString`, which
  recurses on a deep one; `describe` says "a dict of N keys" instead.
- `Shape.json`'s Json <-> PyValue conversions, and the workers' ref
  renamings (`PyWorkers.local`/`refsIn`, `SupervisedWorker.in`/`out`),
  share one bottom-up walk on an explicit stack: `Walk.up`, behind
  `PyValue.refs`/`rebuild`/`rebuildE` (`TestPyValueWalk`).
- The replay loops of `SupervisedWorker` and `RSubprocess` are loops (a
  durable run replays as many steps as it journaled), and
  `PyFacade.scalaType` peels an annotation's wrappers in a loop.

The rest are written bounds: the Go/Haskell/Rust type printers stop at a
product, so a recursive schema cannot loop them; `TsFacade.show` walks a
declaration a person wrote; `R.decode` follows a field's Option nesting;
`RSubprocess.startWith`'s respawn is a stored thunk. Thirteen rows are
paid and deleted, thirteen carry their bound.
