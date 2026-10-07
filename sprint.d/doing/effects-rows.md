- [ ] effects-rows — THE FACADE IN THE CORE, over nominal rows (absorbs
      bang-is-cont; after classic-to-freer, stage 45, 2026-10-07). The
      operator: one `Effects` interface, re-kinded over rows
      (`M[_ <: Row, _]`): `perform` with `Member`, `handle` with `Removed`
      (handlers apply in any order, stage 39), the basic effects as
      methods; the machine's `okay.cont.Free[R, A]` its DIRECT instance,
      the classic an instance through `Union[R, *]` (stage 40, a nominal
      row as the classic union). In the core: `infix type ![A, R <: Row]`,
      `%`, `+` beside `+:` (stage 44, `Pure` the end), the doors
      `pure`/`effect`/`perform`, `handle` over it, `reify`/`reflect` as
      the bridge to the classic. Keep: the same effects in another order
      are one program (`Free.reordered`), `widen` for fewer effects. The
      machine's library (okay-cont: state, reader, writer, throws, choose,
      collect/generate, dialogue — stage 35) is the library of the new `!`.
      Then the FIXED-ROW `Free` (cont-state-cost): `flatMap` at one row,
      one `Has` a run, `inject` by the expected type — measured on
      `rowAnswering`/`rowGeneral` (47.8/64.5 µs per 2000 ops today).
