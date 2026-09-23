- [~] **okay-clojure** — Clojure interop and a Stage IS a transducer, both
      ways (specs/clojure.md): `Clj` (fn/require/eval, refusals by name),
      `Transducers.of(stage)` usable in into/transduce/sequence/comp,
      `Transducers.stage(xf)` for Clojure's own transducers in okay
      pipelines; law tests against `(into [] xf coll)`, short-circuit and
      downstream-reduced tests bounded so a mutant fails rather than hangs.
      Operator ask, 2026-09-23.
