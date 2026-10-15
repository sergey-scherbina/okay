# The platform on the kernel's words: the order of the modules

Companion of [freer-on-core.md](freer-on-core.md). Worked out 2026-10-15 from build.sbt's `dependsOn` (only the
edges to modules that themselves need conversion; test edges marked (t)), counting the files that use okay-freer's
words. okay-std's main is converted (batches 1–4); what of it stays on okay-freer is self-contained interpreters and
non-program types.

## Layers, bottom-up

| L | modules |
|---|---|
| 1 | okay-async (design), okay-kernel, okay-guice, okay-cdi |
| 2 | okay-data |
| 3 | okay-platform |
| 4 | okay-direct (design), okay-stm (design), okay-spring |
| 5 | okay-stream (design), okay-optics, okay-cats (design) |
| 6 | okay-workflow (design), okay-scala2, okay-actor, okay-reactive, okay-lex, okay-parquet |
| 7 | okay-parse, okay-scala2-stm, okay-scala2-workflow, okay-scala2-prelude |
| 8 | okay-codec |
| 9 | okay-http, okay-sql (design), okay-persist, okay-llm (design), okay-py (design), okay-ts, okay-refine, okay-semantic-ossie, okay-conf |
| 10 | okay-ui (design), okay-jdbc, okay-resilience (design), okay-bayes (design), okay-blob, okay-r, okay-clojure, okay-frege, okay-foreign-workflow, okay-rust, okay-r2dbc, okay-ts-npm, okay-semantic-data, okay-desktop, okay-mail, okay-chat, okay-live, okay-x402, okay-deploy, okay-openapi, okay-scala2-http, okay-scala2-llm |
| 11 | okay-pg, okay-delta, okay-semantic-sql, okay-outbox, okay-telegram, okay-x402-evm, okay-ui-gtk, okay-scala2-sql, okay-scala2-ui |
| 12 | okay-rag, okay-docs, okay-x402-signers, okay-x402-cdp |
| 13 | okay-agent (design), okay-cluster, okay-langchain4j-embed, okay-onnx, okay-scala2-rag |
| 14 | okay-mcp, okay-security, okay-intent, okay-kafka, okay-lake, okay-foreign-cluster, compare (its jmh: design), okay-scala2-agent |
| 15 | okay-mcp-http, okay-obs, okay-admin, okay-x402-mcp, okay-spark, okay-java, okay-zio (design), okay-fs2 (design), okay-scala2-mcp |
| 16 | okay-jetty, okay-kyo (design), okay-flink |
| 17 | okay-ops, okay-acme, okay-netty, okay-scala2-ws |
| 18 | okay-script, okay-pool, okay-demo, okay-scala2-services |
| 19 | okay-demo-e2e-browser |

Layers 14–17 are deep through test edges alone; with a module's tests converted after its main they move to 9–13
(okay-jetty, okay-obs, okay-security, and the interop modules hanging on `compare`'s tests).

## Design work, not words: machinery over okay-freer's tree

| module | what |
|---|---|
| okay-async | Async.scala — the async interpreter loop (`Free.Bind`, `resume`); AsyncCont already on the kernel (stepping) |
| okay-direct | the quoted macros emit `Free.Bind`/`Return` trees (DirectRow and the compiler) — they must emit the root's `flatMap` |
| okay-stream | Pipe, Source (HandleFrames), Chunks, Interop, ReadyMerge, Channel — walks over the tree; StreamCont already on the kernel |
| okay-workflow | Proc.scala |
| okay-sql | Tx.scala — the typed region over an indexed tree (its replacement: okay-tx) |
| okay-stm | Stm.scala — `resume` |
| okay-bayes | Bayes, Smooth — walks |
| okay-llm | Llm, OpenAi, Structured — `rest.resume` |
| okay-ui | Dialog, PWizard — a stepped dialog (`Step`) |
| okay-resilience | `resume` |
| okay-agent, okay-py | HandleFrames |
| okay-cats, okay-zio, okay-kyo, okay-fs2 | interop stepping a program (`Step`) |
| compare (jmh) | carrier benchmarks over the tree |

In each: a walk becomes a kernel handler form, or the program stepped (`Step`), as in okay-std.

## okay-std's tests

Convert: the tests of okay-std's APIs (Chronicle, Clock/Random, Gen, Logic, Maybe, Provide/Module, Prob, Refs,
Lexical, Sim/Bisim, State/PState, Std, ReaderBridge, Supply, Throws, TRef, Validated, Writer, Resource, the doc
examples, NamedTuples).

Remove from the branch (they test okay-freer's own machinery and stay at the tag `classic-freer`): Delimited*,
HandleFrames*, HandledStack*, Stepped, Cont*, the ATM and row-cost probes, Row*, Distinct, Tag, Instances, Bind,
Widen*, Free*, BuildShape, BangLoop, freer's stack-safety and stack-pool tests, Shift*, Prog, Handler* forms,
HandleForward, HandlersAsDollar, FoldCont, Effects/Eager/Monadic/FreerPara, Fused, Classes/Laws/Aggregate/FoldMap/
FoldUntil, SeqEffect, Static.
