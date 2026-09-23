## free-answer-upcast - `Free[F, +A]`: the answer is covariant, three identity-map doors gone

`Free` was invariant in its answer as in its row, and the answer axis
had no coercion, so three doors paid a `Bind` per program to move a
GADT-proved `A' <: A` up: `SharedOnce.answer` (twice) and okay-workflow's
`Wf.up` (nine call sites). Now `enum Free[F[+_], +A]`: the doors are
deleted, `Int ! F` is an `Any ! F` for free (TestFreeVariance, the
same object), and `javap -c -p` of the seven core classes shows 0
differing lines — the annotation is signature-only, so there was
nothing to benchmark. The row stays invariant (free-row-variance's
measured decision, pinned by a `compileErrors`). Price, four sites
where inference leaned on invariance: `runEither` pins its `B`, a
GADT match in a `direct` block whose first arm is concrete is
ascribed `: X` (TestDirectOnce), and the `direct` macro compares
elements by `<:<` (an inlined `raise` is now an `Inject[…, Nothing]`).
specs/writer-covariance.md "free-answer-variance"; docs: theory ch. 4,
typepedia, guide §2.
