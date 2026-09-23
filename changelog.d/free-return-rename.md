## free-return-rename - the answer node is `Return`, and nothing shadows `Pure` any more

The operator's word (2026-09-23): "Free.Pure переименуем в Free.Return
— мне и раньше казалось что так будет лучше звучать". Three things
had been named `Pure`: the empty ROW (`A ! Pure`, = `Nothing`), the
tree's answer NODE (`Free.Pure(a)`), and `Cont.Pure`. The node is now
`Free.Return(a)`; the row and the lifting function `pure(a): A ! F`
stay; `Cont.Pure` stays (it is always written qualified and was never
the collision).

- `enum Free`: `case Return(a: A)`. 42 files: every `case Pure(` under
  `import okay.!.*` is `case Return(`, every unqualified constructor
  `Pure(x)` is `Return(x)`, every `Free.Pure` is `Free.Return`, the
  selective imports name `Return`. `Cont.scala`'s own `def Pure` (the
  facade's constructor over `Free.Return`) is untouched.
- `Effects.scala`'s note "in scopes that import !.* write okay.Pure
  (the Free.Pure case shadows it)" was true and is gone; `okay.Pure`
  still compiles everywhere it was written.
- docs: theory ch. 4, 5, 11, typepedia, continuations appendix A,
  two specs — the three head forms read `Return(a)`, `Inject(e)`,
  `Bind(Inject(e), k)`.

Mechanical, compiler-checked: the case was renamed first and the
compiler listed every site; one gate over the whole family.
