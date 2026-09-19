- [ ] **direct-compiler-phases** — `Direct.compileAll` (Direct.scala
      641–2105) is one 1460-line method of ~70 nested defs sharing a
      closure over the Quotes, the monad and the mode flags. Not
      broken; but no phase has a name, so none can be tested apart
      from the whole macro's behaviour.

      HOW: the closure becomes `private[okay] final class
      DirectCompiler[F]`, mixed from one trait per phase in its own
      file — DirectMarks, DirectRow, DirectEmit, DirectDefer,
      DirectVals, DirectLoops, DirectParallel — over a base
      `DirectPhase` that owns the Quotes given (at `q.type`), `Out`,
      and the abstract knot `compile`/`compileBlock`. Terms cross
      Quotes paths as `Expr`. Emission unchanged by construction.
      Test-side macros `DirectProbe` run one phase and answer with
      data; `TestDirectPhases` asserts each. spec:
      specs/direct-macro.md "Structure".

      DONE WHEN: every TestDirect* suite green unchanged, the phase
      probes green, gate `affected master` green, changelog entry.
