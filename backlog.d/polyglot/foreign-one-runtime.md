- [ ] foreign-one-runtime — stage 4 of specs/foreign-one.md: the caller's
      API as ONE thing. `Runtime[L <: Lang]` (call/hold/program/stream/
      run/release, `speaks`), `Module[L]`, `Address[L]`, `Arg[L]`/`Ret[L,
      Out]` (a value, a Table, a Ref, a Chunks — each on its own road,
      each by a given that needs the language's MARKER), the markers
      `Tables[L]`/`Objects[L]`/`Methods[L]`/`Programs[L]`(`.MultiShot`)/
      `Streams[L]` as compile-time claims (foreign-facade's typeclasses
      had a body per language because there were three engines; with one
      engine the body exists once and only the claim differs), two
      runtimes (`WireRuntime[L]` over the engine + `Language[L]`;
      `JvmRuntime` over the `okay.Foreign` walker — Clojure/Frege programs
      are `Programs[Jvm]`, narrowing foreign-facade Decision 7). The
      cluster's combinators become ONE body each over these: `mapIn`,
      `Reduce.in`, `statefulIn`, `Model.in`/`mapModel`, `flow.through` —
      `PyStage`/`RStage`, `PyReducer`/`RReducer`, `PyStreamer`/`RStreamer`,
      `PyModel`/`RModel`, `mapPy`/`mapR`/`Reduce.py`/`Reduce.r` deleted;
      their APIs unchanged. Module `okay-foreign` holds the model; okay-py
      and okay-r shrink to their `Language[L]`; `okay.py`/`Py.*`/`R.*`
      stay as aliases one release. WAITS for foreign-reduce and
      foreign-streams-holds to land (it changes their bodies, not their
      API). Subsumes the earlier foreign-one-modules. Gate:
      FacadeConformance — one body, a subclass per runtime (Py, R, Rust,
      Go, Hs, Ts, Jvm); compileErrors per missing marker.
