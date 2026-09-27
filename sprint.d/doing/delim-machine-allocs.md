- [ ] delim-machine-allocs — the `Delim` machine allocates three
      things per operation that carry no information, and every
      FOREIGN operation under `Delim.run` pays 2x for it
      (`writerTellUnderDelim` 2.01x/2.27x `writerTell`, delim-guard-
      per-op 2026-09-17; `delimPushOnly` 358 B per push). Delim.scala:
      (1) `step` (:1022) answers `Either[R ! F, Next]` — a `Right` per
      operation and a `Next` per operation; make `Next` the answer and
      the finished program a sentinel case (or a field on the run) so
      the loop tests one thing and allocates one; (2) every `Push`,
      `Dollar`, `Watched` (:1029, :1072, :1085) adds an identity `K`
      frame plus its closure ONLY to re-type the prompt's `r` to the
      operation's `X` — carry that as a typed witness on the `Mark`/
      `Ret` frame instead (the compiler already has `r =:= X` at that
      line), which also drops one frame from every capture copy and
      one loop step from every normal return; (3) a foreign operation
      (:1089) resumes as `loop(Next(pure(x), kont))` — a `Return`, a
      `resume` and a `kont` match to reach `K(f, rest)`; when the head
      is a `K`, continue as `Next(f(x), rest)` directly. Each is its
      own commit with its own `-prof gc` row (`DelimBenchmark`:
      `delimPushOnly`, `delimDollarOnly`, `delimDollarResume`,
      `delimGenerator`, `writerTellUnderDelim`; one lane per
      `Jmh/run`), so a change that does nothing is reverted alone.
      EXPECTED (hypothesis): `delimPushOnly` -15% B, `writerTellUnder
      Delim` from 2.0x to <=1.6x, `delimGenerator` (902 328 B) not
      worse — its capture copies one frame fewer. Laws: every Delim
      suite, `TestDelimStackSafety` (the 20 000-delimiter split and
      the nested-capture depth), `lexical-tail-guard-abort`'s shots
      count. Refuted already, do not retake: a `var` on every machine
      frame (3-4% on non-capturing lanes), `Option` per frame in
      `split`, the single-`Cut` shape (+104 B/capture), an eager
      prompt label (+21%). SECOND HALF, a lane and not a change: no
      benchmark varies CAPTURE DEPTH — `split` copies and `reify`
      re-walks every frame between the shift and its prompt on EVERY
      call of k, and `delimGenerator` has ~1 such frame. Add
      `delimCaptureDepth` with N = 1/16/256 frames under the prompt
      (a `flatMap` chain, then `shift`, k called once and 8 times):
      the slope is what `continuations-as-data-spike` (backlog) needs
      a verdict against, and Logic's search (`choose` captures through
      everything since its prompt) is the consumer whose cost it
      names. Spec: specs/delimited-control.md, Results + a Decision
      per item. (2026-09-27, perf-plan)
