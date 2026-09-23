## scala2-choose-search - nondeterminism, fair search and Search for Scala 2.13; residual-row-typeable invented and closed

- `okay.scala2.Choose` makes okay's `Choose`/`Logic` a capability of
  `Eff`: `from`, `fail`, `guard`; handlers `all`, `first` (lazy, so the
  search may be infinite), `cut`, `ifte`; and the FAIR `interleave` and
  `fairBind`. `okay.scala2.Search` provides `bestOf`, `all` and
  `majority` over any `Eff`, which is okay-agent's "sample until valid"
  (`gen` can be `chat.say(...)`).
- residual-row-typeable (backlog, operator: "выдумай"), invented and
  closed. The fair combinators need a `TypeableK` for the rest of the
  row, which on the Scala 2 side is only a phantom. The facade gives
  the COMPLEMENT of the known side, "not a `Choose`". Reading
  `Logic.scala` showed that the fair combinators never consult that
  instance at all: their only split is `msplit`'s, on the `Choose` side.
  So nested splits cannot misroute. The other three properties the
  backlog asked for are tests: a Writer in the rest of the row passes
  through `interleave` in order; State is per branch inside the search
  and shared outside it; and an infinite branch does not starve the
  other one.
- The `R = Any` lint trap returned for combinators that KEEP the
  capability. It is fixed by taking the row as `R <: Choose` rather
  than `Choose & R`, which is cleaner than stage 3's separate `run`.
- `TestChooseFromScala2` has 7 tests, green. Docs: section 8h of
  docs/scala2.md (copied from the probe), API reference, typepedia
  (including the residual instance in the claims registry), and spec
  stage 13.
