- [ ] foreign-engine-typeclass — one API over the polyglot map-reduce
      (operator, 2026-09-25: "какую то удобную фасад абстракцию поверх
      этого? Чтобы один и тот же код работал со всем этим прозрачно через
      имплиситы и тайпклассы"). `Engine[M]`, a typeclass by the MODULE's
      type: `Engine[PyModule]`, `Engine[RModule]`, `Engine[JvmModule]`
      (Scala, Clojure, Frege as functions by name, in the JVM); one
      surface, `flow.mapIn(module, "fn")` and `Reduce.in(module, "step",
      "merge")`, the engine chosen by the implicit, the interpreter by a
      given (`Engine.py(python)`); `mapPy`/`mapR`/`Reduce.py`/`Reduce.r`
      become the cases. specs/foreign-map-reduce.md stage 3.
