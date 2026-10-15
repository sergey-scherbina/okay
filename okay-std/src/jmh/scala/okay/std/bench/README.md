# okay-std's JMH lanes: classic road vs new road

Classic `okay.freer.X.m` is at the git tag `classic-freer` (okay-freer's words). The new `okay.std.bench.X.m` is
on this branch (the root's words: `A ! Es` a value over okay-core's one monad). The two share the class name, the
method name, `@Param` and the JMH annotations, so their results line up by `X.m`.
"ported" means the same program, the same n, effects and handler order. "NEAREST" means the work differs, and
a comment on the method says how. "none" means the method measured okay-freer's own machinery.

| classic method | new counterpart |
|---|---|
| `BuildShapeBenchmark.tellsFoldLeft` | `okay.std.bench.BuildShapeBenchmark.tellsFoldLeft` |
| `BuildShapeBenchmark.tellsEach` | `okay.std.bench.BuildShapeBenchmark.tellsEach` — `each` a local copy of freer's, on std `foldM` |
| `BuildShapeBenchmark.stateFoldLeft` | `okay.std.bench.BuildShapeBenchmark.stateFoldLeft` |
| `BuildShapeBenchmark.stateFoldM` | `okay.std.bench.BuildShapeBenchmark.stateFoldM` — std `foldM` (no freer map fusion) |
| `BuildShapeBenchmark.rowFoldLeft` | `okay.std.bench.BuildShapeBenchmark.rowFoldLeft` |
| `BuildShapeBenchmark.rowFoldM` | `okay.std.bench.BuildShapeBenchmark.rowFoldM` — std `foldM` (no freer map fusion) |
| `BuildShapeBenchmark.rowOneBind` | `okay.std.bench.BuildShapeBenchmark.rowOneBind` |
| `BuildShapeBenchmark.stateOneBind` | `okay.std.bench.BuildShapeBenchmark.stateOneBind` |
| `BuildShapeBenchmark.stateUpdate` | `okay.std.bench.BuildShapeBenchmark.stateUpdate` — std `foldM` (no freer map fusion) |
| `BuildShapeBenchmark.readerLift` | `okay.std.bench.BuildShapeBenchmark.readerLift` — std `foldM` (no freer map fusion) |
| `BuildShapeBenchmark.rowUnwrap` | none (freer internal): matches freer's `Bind`/`Mapped` nodes |
| `BuildShapeBenchmark.rowFoldMAcc` | `okay.std.bench.BuildShapeBenchmark.rowFoldMAcc` — std `foldM` |
| `BuildShapeBenchmark.rowOneBindAcc` | `okay.std.bench.BuildShapeBenchmark.rowOneBindAcc` |
| `BuildShapeBenchmark.stateFoldEach` | `okay.std.bench.BuildShapeBenchmark.stateFoldEach` — `foldEach` a local copy of freer's library body |
| `BuildShapeBenchmark.stateFoldEachInt` | `okay.std.bench.BuildShapeBenchmark.stateFoldEachInt` — `defer` for `Free.delay` |
| `BuildShapeBenchmark.stateOneVecBind` | `okay.std.bench.BuildShapeBenchmark.stateOneVecBind` |
| `BuildShapeBenchmark.stateFoldEachArr` | `okay.std.bench.BuildShapeBenchmark.stateFoldEachArr` — `defer` for `Free.delay` |
| `BuildShapeBenchmark.stateFoldEachInline` | `okay.std.bench.BuildShapeBenchmark.stateFoldEachInline` — `defer` for `Free.delay` |
| `BuildShapeBenchmark.stateFoldMInline` | none (freer internal): matches freer's `Bind`/`Mapped` nodes |
| `ClassicLoopBenchmark.classic` | `okay.std.bench.ClassicLoopBenchmark.classic` — NOT in the tag `classic-freer` (added after it): the classic side builds at `8a4462ee0` (it was added there; okay-std still on okay-freer) |
| `DelimBenchmark.writerTell` | `okay.std.bench.DelimBenchmark.writerTell` |
| `DelimBenchmark.delimGenerator` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimPushOnly` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimDollarOnly` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimMacroOnly` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimDollarResume` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimRunEach` | none (freer internal): freer `Shift` |
| `DelimBenchmark.delimMacroResume` | none (freer internal): freer `Shift` |
| `DelimBenchmark.layeredViaDollar` | none (freer internal): freer `Shift` |
| `DelimBenchmark.layeredViaPush` | none (freer internal): freer `Shift` |
| `DelimBenchmark.stateHandle` | `okay.std.bench.DelimBenchmark.stateHandle` |
| `DelimBenchmark.stateDeep` | none (freer internal): freer `resume` + `Shift` |
| `DelimBenchmark.stateForeign` | none (freer internal): freer `Shift.run` |
| `DelimBenchmark.stateLexTail` | none (freer internal): freer `Lexical` (LexicalState) |
| `DelimBenchmark.stateLexTailPure` | none (freer internal): freer `Lexical` (LexicalState) |
| `DelimBenchmark.stateLexTailPureInMachine` | none (freer internal): freer `Lexical` (LexicalState) |
| `DelimBenchmark.stateLexWalk` | none (freer internal): freer `Lexical` (LexicalState) |
| `DelimBenchmark.stateLexDeep` | none (freer internal): freer `Lexical` (LexicalState) |
| `DelimBenchmark.writerTellUnderDelim` | none (freer internal): freer `Shift` |
| `DelimBenchmark.plainList` | `okay.std.bench.DelimBenchmark.plainList` — no effects, same code |
| `DispatchBenchmark.chain` | none (freer internal): `TypeableK`, `resume`, `HandleFrames` |
| `DispatchBenchmark.table` | none (freer internal): `TypeableK`, `resume`, `HandleFrames` |
| `FibBenchmark.fib10` | `okay.std.bench.FibBenchmark.fib10` — NEAREST — the kernel's `Step` (std iterator) instead of freer `next`/`peek` |
| `FibBenchmark.fib50` | `okay.std.bench.FibBenchmark.fib50` — NEAREST — the kernel's `Step` (std iterator) instead of freer `next`/`peek` |
| `FibBenchmark.fib100` | `okay.std.bench.FibBenchmark.fib100` — NEAREST — the kernel's `Step` (std iterator) instead of freer `next`/`peek` |
| `FibBenchmark.fib1000` | `okay.std.bench.FibBenchmark.fib1000` — NEAREST — the kernel's `Step` (std iterator) instead of freer `next`/`peek` |
| `FusionBenchmark.ctrlContSWr` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.ctrlFuncSWr` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.blockFuncInlineHR` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.blockFuncInlineMatchR` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.blockFreeR` | none (freer internal): `Fused.stateWriter` fixture |
| `FusionBenchmark.blockFreeBuildR` | none (freer internal): `Fused.stateWriter` fixture |
| `FusionBenchmark.nestedBlockR` | `okay.std.bench.FusionBenchmark.nestedBlockR` |
| `FusionBenchmark.blockFuncR` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.blockContR` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.blockFuncStagedR` | none (freer internal): `Control` carriers `Cps`/`Func` |
| `FusionBenchmark.nestedSW` | `okay.std.bench.FusionBenchmark.nestedSW` |
| `FusionBenchmark.nestedWS` | `okay.std.bench.FusionBenchmark.nestedWS` |
| `FusionBenchmark.fusedSW` | none (freer internal): `Fused` |
| `FusionBenchmark.nestedSWr` | `okay.std.bench.FusionBenchmark.nestedSWr` |
| `FusionBenchmark.fusedSWr` | none (freer internal): `Fused` |
| `FusionBenchmark.nestedTSW` | `okay.std.bench.FusionBenchmark.nestedTSW` |
| `FusionBenchmark.handledSW` | `okay.std.bench.FusionBenchmark.handledSW` |
| `FusionBenchmark.handledSWr` | `okay.std.bench.FusionBenchmark.handledSWr` |
| `FusionBenchmark.handledTSW` | `okay.std.bench.FusionBenchmark.handledTSW` |
| `FusionBenchmark.fusedTSW` | none (freer internal): `Fused` |
| `HandlerBenchmark.relayForward` | none (freer internal): `relay` |
| `HandlerBenchmark.buildOnly` | `okay.std.bench.HandlerBenchmark.buildOnly` |
| `HandlerBenchmark.relayPrebuilt` | none (freer internal): `relay` |
| `HandlerBenchmark.handleForward` | `okay.std.bench.HandlerBenchmark.handleForward` — `Handler.answering`; Produce by `Stream.runWith` |
| `HandlerBenchmark.handleForwardCps` | none (freer internal): CPS carrier |
| `HandlerBenchmark.handlePrebuilt` | `okay.std.bench.HandlerBenchmark.handlePrebuilt` |
| `HandlerBenchmark.handlePrebuiltCps` | none (freer internal): CPS carrier |
| `HandlerBenchmark.handleCapture` | `okay.std.bench.HandlerBenchmark.handleCapture` — `Handler.deep`, `k` resumed |
| `HandlerBenchmark.handleCaptureCps` | none (freer internal): CPS carrier |
| `HandlerBenchmark.tailcallChain` | `okay.std.bench.HandlerBenchmark.tailcallChain` |
| `HandlerBenchmark.stepBulk` | `okay.std.bench.HandlerBenchmark.stepBulk` — NEAREST — the kernel's `Step` (std iterator) instead of freer `next(N)` |
| `HandlerBenchmark.stepOneByOne` | `okay.std.bench.HandlerBenchmark.stepOneByOne` — NEAREST — `uncons` N times instead of freer `next(1)` N times |
| `HandlerBenchmark.stateEffect` | `okay.std.bench.HandlerBenchmark.stateEffect` |
| `HandlerBenchmark.writerTell` | `okay.std.bench.HandlerBenchmark.writerTell` |
| `HandlerBenchmark.stateSmallHandle` | `okay.std.bench.HandlerBenchmark.stateSmallHandle` |
| `HandlerBenchmark.stateSmall` | `okay.std.bench.HandlerBenchmark.stateSmall` |
| `HandlerBenchmark.contAnswer` | none (freer internal): `Cps` alone |
| `HandlerBenchmark.contAnswerStrict` | none (freer internal): `Cps` alone |
| `HandlerBenchmark.statePara` | none (freer internal): `PState` (Cps) |
| `HandlerBenchmark.stateThreaded` | none (freer internal): `PState.Threaded` (freer indexed tree) |
| `HandlerBenchmark.stateForward` | `okay.std.bench.HandlerBenchmark.stateForward` — NEAREST — the Ask residue run by `Handler.answering`, not freer's `askLoop` |
| `HandlerBenchmark.stateIndexedForward` | none (freer internal): `handleIndexed` |
| `HandlerFormsBenchmark.tick_cases` | `okay.std.bench.HandlerFormsBenchmark.tick_cases` — NEAREST — the case form as freer's macro expands it, over `Handler.answering`/`state` |
| `HandlerFormsBenchmark.tick_poly` | `okay.std.bench.HandlerFormsBenchmark.tick_poly` |
| `HandlerFormsBenchmark.count_cases` | `okay.std.bench.HandlerFormsBenchmark.count_cases` — NEAREST — the case form as freer's macro expands it, over `Handler.answering`/`state` |
| `HandlerFormsBenchmark.count_poly` | `okay.std.bench.HandlerFormsBenchmark.count_poly` |
| `HandlerFormsBenchmark.reader_builtin` | `okay.std.bench.HandlerFormsBenchmark.reader_builtin` |
| `HandlerFormsBenchmark.reader_answer` | `okay.std.bench.HandlerFormsBenchmark.reader_answer` |
| `HandlerFormsBenchmark.state_builtin` | `okay.std.bench.HandlerFormsBenchmark.state_builtin` |
| `HandlerFormsBenchmark.state_form` | `okay.std.bench.HandlerFormsBenchmark.state_form` |
| `HandlerFormsBenchmark.maybe_builtin` | `okay.std.bench.HandlerFormsBenchmark.maybe_builtin` |
| `HandlerFormsBenchmark.maybe_control` | `okay.std.bench.HandlerFormsBenchmark.maybe_control` |
| `LogicBenchmark.observe_tree` | `okay.std.bench.LogicBenchmark.observe_tree` — slows down badly on this road, see LogicBenchmark's header |
| `LogicBenchmark.shared_scope` | `okay.std.bench.LogicBenchmark.shared_scope` |
| `ShiftBenchmark.shift0_seq` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.shift_seqDF` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.cont_seq` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.cont_strict_seq` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.delim_seq` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.abort_try` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.abort_scope` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.shift0_twoShot` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.cont_twoShot` | none (freer internal): freer `Shift`/`Cps` |
| `ShiftBenchmark.delim_twoShot` | none (freer internal): freer `Shift`/`Cps` |
| `SplitBenchmark.writerShip` | `okay.std.bench.SplitBenchmark.writerShip` |
| `SplitBenchmark.writerEither` | none (freer internal): `resume` + `<|>` |
| `SplitBenchmark.writerExtract` | none (freer internal): `resume` + extractor |
| `SplitBenchmark.writerShipVec` | `okay.std.bench.SplitBenchmark.writerShipVec` |
| `SplitBenchmark.mixedList` | `okay.std.bench.SplitBenchmark.mixedList` |
| `SplitBenchmark.mixedVec` | `okay.std.bench.SplitBenchmark.mixedVec` |
| `SplitBenchmark.mixedListNoRev` | `okay.std.bench.SplitBenchmark.mixedListNoRev` |
| `SplitBenchmark.mixedVecInline` | `okay.std.bench.SplitBenchmark.mixedVecInline` |
| `SplitBenchmark.writerMut` | none (freer internal): a local freer loop |
| `SplitBenchmark.mixedMut` | none (freer internal): a local freer loop |
| `SplitBenchmark.stateShip` | `okay.std.bench.SplitBenchmark.stateShip` |
| `SplitBenchmark.stateEither` | none (freer internal): `resume` + `<|>` |
| `SplitBenchmark.stateExtract` | none (freer internal): `resume` + extractor |
| `StagedBenchmark.cont24` | none (freer internal): `Cps` carrier |
| `StagedBenchmark.func24` | none (freer internal): `Func` carrier |
| `StagedBenchmark.effCont24` | `okay.std.bench.StagedBenchmark.effCont24` — `Classic[Free]` → the root's `flatMap`/`produce` |
| `StagedBenchmark.effInline24` | none (freer internal): `Interpr` over `Func` |
| `ValidatedBenchmark.validatedAllValid` | `okay.std.bench.ValidatedBenchmark.validatedAllValid` — no program, same code |
| `ValidatedBenchmark.eitherAllRight` | `okay.std.bench.ValidatedBenchmark.eitherAllRight` — no program, same code |
| `ValidatedBenchmark.validatedAllInvalid` | `okay.std.bench.ValidatedBenchmark.validatedAllInvalid` — no program, same code |

130 classic methods: 59 ported, 9 nearest equivalents, 62 with no counterpart.
