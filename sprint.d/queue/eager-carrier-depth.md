- [ ] eager-carrier-depth — `tailRecM` (TailRecM, Effects.scala) and
      `foldMap` into an EAGER carrier (`Option`, `Either`, a strict `Box`)
      are NOT host-stack-free, as their comments, specs/effects-foldmap.md,
      specs/monad-tailrecm.md, docs/contract.md, docs/typepedia.md and
      docs/interop-classes.md now claim (operator: "очень интересный
      вопрос", 2026-10-02). MEASURED during okay-cats-cross: an eager
      flatMap calls the continuation inside the shift body, so the Cont
      runner nests; on the JVM it survives by moving to a fresh stack
      (specs/cont-stack.md, StackSwitch) — a 1 MB or 8 MB thread folds
      1 000 000 iterations, but a 128 KB thread overflows already at 1 000
      (the first segment's room exceeds what that stack holds); on
      Scala.js there is no fresh stack and the engine overflows between
      300 and 1 000 iterations ("RangeError: Maximum call stack size
      exceeded"). The earlier "a million through Option" results ran on
      sbt's -Xss8m / a forked main thread. TO DO: correct every claim;
      find whether an eager carrier can loop in constant stack WITHOUT
      its cooperation (e.g. detect a synchronous resume of `k` inside the
      shift body and continue the loop instead of nesting — the
      callback drive's Got/Moved exchange already does this for Await),
      measure it on JVM (SmallStack 128 KB), Native and JS; if not, write
      the bound and require a native tailRecM for eager carriers (cats'
      answer). TestCatsBridges' "a million through an eager okay monad"
      is JVM-only until then.
