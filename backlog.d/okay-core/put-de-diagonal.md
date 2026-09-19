- [ ] put-de-diagonal — `Put[F[_]]` in `src/main/scala/Generate.scala`
      is DIAGONAL: `put[A](a: A): A /> F[A]` forces the carrier to be
      indexed by the ELEMENT type and to answer with it, which is why
      the third instance is `Teller[A] = A ! Writer % A` and why no real
      seam ever took one — a real source is `Source[W] = Unit ! (Writer
      % W + Async)`, answer `Unit`, element `W`. Measured 2026-09-19:
      outside Generate.scala, ZERO production call sites of `Put`,
      `take`, `put` or `generate` (tests: TestGenerate, TestStream,
      TestStd; benchmarks: GeneratorBenchmark, FibBenchmark,
      HandlerBenchmark); `Chunks.generate`/`nats`/`fibs` do not go
      through `Put` either, they are hand-written over `produce`.
      THE CHANGE: `trait Put[S[_]]: def put[W](w: W): Unit /> S[W]` —
      the answer decoupled from the element. Every present instance
      survives it (`LazyList`: `shift(w #:: _(()))`; `Producer`:
      `shift(k => produce(w).flatMap(_ => k(())))`; the writer stream
      `[W] =>> Unit ! Writer % W` replaces the diagonal `Teller`
      instance), and `Source[W]` GAINS one (`Writer.tell(w)` then
      `k(())`), so `generate`/`nats`/`fibs` produce an async source for
      free and generator code is written against `Put[S]` + consumed
      against `Stream[S, F]` without naming a carrier. `generate`'s
      body is unchanged: `for a <- take[A, S[B]]; _ <- put(f(a)) yield
      g(a)`. Lost: `put` no longer echoes its argument for chaining —
      no caller used that. Then: one section in `docs/guide.md` beside
      Producer ("one unfold, four carriers", pointing at
      docs/theory and docs/continuations where `take`/`put` are
      explained today — guide.md does not mention them at all), and
      the `nats`/`fibs` pair in `Chunks` re-expressed through the
      generic ones or documented as the chunked specialisation. Gate:
      TestGenerate extended with the Source carrier (watch it FAIL on
      the old signature first), TestStd/TestStream, the three
      benchmarks recompiled. Pairs with [[producer-to-writer-carrier]]:
      that lane decides whether the `Producer` instance stays.
