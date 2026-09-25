- [ ] foreign-one-ops — stage 4 of specs/foreign-one.md: one effect
      declaration. On the wire an effect is a `Cb[F]` with `Schema`s and
      typed stubs are GENERATED (`Rs.ops`, `Go.ops`, `Hs.ops`, `Ts.ops`);
      in the JVM family Frege and Clojure bind `okay.frege.Ops`/
      `okay.clojure.Ops` BY HAND, so "declared once in Scala" is false for
      half the languages. `Frege.ops(name, cbs)` writes the `native`
      bindings and `Operation a` constructors a Frege module imports,
      `Clj.ops` the `defn`s of a namespace; the shipped `okay.frege.Ops`
      and `okay.core` become the generated output for the core effects
      (regenerate, diff empty). `Member[F]` stays the runtime test; the
      declaration is what moves. Gate: a Frege program performing a
      generated typed op under the caller's Reader, a wrong argument a
      Frege TYPE error (hs-typed-effects' shape); Clojure the same with a
      runtime refusal by name. Last of the five: it only moves a
      declaration.
