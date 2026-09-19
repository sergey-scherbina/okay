## producer-writer-carrier-foldwriter-fix - the foldWriter gap, actually diagnosed

The previous lane (producer-writer-carrier-chunked-fold) left
`Chunks.foldWriter` 3.5-6.8x slower than `Chunks.fold`, with a comment
saying the diagnosis would need a profiler the environment didn't
have. That claim was wrong: JMH ships `-prof gc` and `-prof jfr` (Java
Flight Recorder) in-JDK, and `javap` reads compiled bytecode directly —
no external tool needed. This lane used them.

**The actual cause:** `Fold.OfLong[A]`'s `addLong(s: Long, a: A): Long`
takes its element generically by design (a fold over `A`, not
necessarily `Long`), so every call boxes it through a synthetic bridge.
`-prof gc` showed the dispatched fold allocating 249,584 B/op against
12,656 for a direct call — about 10,000 boxed `java.lang.Long`, one per
element. `-prof jfr`'s allocation stack traces name it exactly:
`ArraySeq$ofLong.apply` -> `boxToLong` -> `Fold$OfLong.addLong`.
`javap` then showed `Chunks.fold` makes the IDENTICAL call and pays
nothing, because the JIT's escape analysis eliminates the box inside
`Chunks.foldLeft`'s small, standalone compiled loop — the same analysis
fails inside `Writer.foldWith`'s bigger resume/split/Bind tailrec
trampoline, so the box becomes a real allocation there.

**Two fix attempts, both ruled out with evidence, not guesswork:**
raising `-XX:MaxInlineLevel`/`-XX:FreqInlineSize` well past their
defaults changed nothing (still exactly 249,584 B/op) — not an
inlining-budget problem. Extracting the per-chunk consuming loop into
its own small, standalone method also changed nothing: `javap` showed
the JIT re-inlining it straight back into the trampoline anyway,
reproducing the identical compiled unit either way. That attempt is
reverted rather than kept as dead code with no payoff.

**Still open:** the real fix needs `Writer`'s `Stream` instance to grow
its own `.iterator` override, decoupling the tree walk from the
per-chunk consumption the way `Chunks.foldLeft` already does — blocked
on an API-contract question (`.iterator` needs a `Handler[G]` and runs
eagerly; `foldWriter` currently returns a suspended, composable
program), not a quick patch. `Bulk.scala`, `Pipeline.scala`, and
`Acceptance.scala` stay on `Producer`/`Chunks.fold` until it closes.
Full diagnosis in `Chunks.scala`'s `foldWriter` doc and
`specs/producer-to-writer-carrier.md`'s Results follow-up, so a next
attempt starts from what's ruled out instead of repeating it.

Gate: `okayStreamJVM/test` 346/346 green, 0 warnings; JS/Native compile
clean. No functional or API change — this lane is diagnosis only.
