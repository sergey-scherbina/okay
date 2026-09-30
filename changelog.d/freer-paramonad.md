## freer-paramonad — Freer is the ParaMonad instance for every signature; an indexed effect's index is an answer type, never a consumed state

The operator's ask ("make Freer a ParaMonad instance", and behind it:
can an effect carry Freer's extra parameters, must PState be rebuilt
from the erased Free?). `object Freer` gains `Para[G] = [A, S, R] =>>
Freer[G, S, R, A]` and `given ParaMonad[Para[G]]` (pure = Return,
flatMap = prefix Bind, map = the Mapped bind); `Monad[Free[F, *]]` and
the diagonal bridge still resolve beside it. TestFreerPara answers the
question by compiling: `PState` already sits on the indexed tree (Cont's
Rep is `Freer[Shift, S, R, A]` since 2026-09-29; only the unary row
erases to Unit), and a THREE-ARY signature — `PSt[S => Z, S => Z, S]`,
typestate as data — is handled by an indexed natural transformation
into shift bodies through Cont's runner, GADT-typed, `Int -> String ->
List[String]` moving on the tree. The other reading, the index as a
state the handler consumes (McBride's IxFree), is refused on this base:
`+R` (there for `tailShift`) turns the GADT's equalities into bounds
the wrong way round at every arm that reads the state — pinned as a
compileErrors with two errors. Records: specs/freer-base.md "Freer as
the ParaMonad", docs/theory/03-parameterised.md, docs/typepedia.md.
Prog/Delim.Stacked stay phantom; the three-ary row algebra stays out
of scope (one natural transformation handles a three-ary signature).
Gate: additive — TestFreerPara 6/6 + `affected master Test/compile`.
