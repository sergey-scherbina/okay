package okay

import okay.!.*
import okay.Row.at

/**
 * specs/scoped-effects-laws.md: `recover`×`State`'s order (global by
 * default, scoped on request — decided already by `recover`'s own
 * definition, pinned here rather than left folklore) and `Reader.
 * local`'s laws, including the one case the paper this cites (Kiselyov,
 * Shan & Sabry, ICFP 2006) warns is a genuine trap and this library
 * does not fix.
 */
class TestScopedEffects extends munit.FunSuite:

  // ---------------------------------------------------------- recover x State

  /** `recover` leaves `Throws` in the row unresolved even when it
   * guarantees success (its own type signature says so); discharge it
   * with `runEither` before `State.run`, which wants `State % S` alone */
  def runBoth[A](s: Int)(p: A ! State % Int + Throws % String): (Int, Either[String, A]) =
    State.run(s)(runEither[A, State % Int, String](p))

  test("a State mutation before a caught raise survives: recover does not roll it back") {
    type Row = State % Int + Throws % String
    val p: Int ! Row =
      State.set[Int](5).at[Row].flatMap(_ => raise[String, Int]("boom").at[Row])
        .recover(_ => State.get[Int].at[Row])
    assertEquals(runBoth(0)(p), (5, Right(5)), "the set(5) before the raise is not undone")
  }

  test("recover's handler sees the state LEFT BY the failed attempt, not a snapshot from before") {
    type Row = State % Int + Throws % String
    val p: Int ! Row =
      State.set[Int](1).at[Row]
        .flatMap(_ => State.set[Int](2).at[Row])
        .flatMap(_ => raise[String, Int]("boom").at[Row])
        .recover(_ => State.get[Int].at[Row])
    assertEquals(runBoth(0)(p), (2, Right(2)), "recover saw 2, the last state before the raise, not 0 or 1")
  }

  test("save/restore idiom: a transactional retry over GLOBAL state, one line each side") {
    type Row = State % Int + Throws % String
    def attempt(fail: Boolean): Int ! Row =
      State.modify[Int](_ + 1).at[Row].flatMap { s =>
        if fail then raise[String, Int]("boom").at[Row] else pure[Row, Int](s)
      }
    val p: Int ! Row =
      State.get[Int].at[Row].flatMap { s0 =>
        attempt(fail = true).recover { _ =>
          State.set[Int](s0).at[Row].flatMap(_ => attempt(fail = false))
        }
      }
    val (finalState, result) = runBoth(0)(p)
    assertEquals(result, Right(1), "the retry succeeded")
    assertEquals(finalState, 1, "restored to s0 before the retry, then advanced once by the retry — not twice")
  }

  test("a State.run applied INSIDE the guarded scope isolates its writes completely") {
    type Inner = State % Int + Throws % String
    val (outerFinal, (b, innerFinal)) = State.run(10)(
      State.get[Int].flatMap { b =>
        val innerP: Unit ! Inner =
          State.set[Int](999).at[Inner].flatMap(_ => raise[String, Unit]("boom").at[Inner])
            .recover(_ => State.get[Int].at[Inner].map(_ => ()))
        val (innerState, _) = runBoth(0)(innerP)
        pure[State % Int, (Int, Int)]((b, innerState))
      })
    assertEquals((outerFinal, b, innerFinal), (10, 10, 999),
      "the outer state (10) never moved; the inner run's own 999 is entirely its own business")
  }

  test("guide: scoped effects — the two pinned examples, verbatim") {
    type Row = State % Int + Throws % String
    val p: Int ! Row =
      State.set[Int](5).at[Row].flatMap(_ => raise[String, Int]("boom").at[Row])
        .recover(_ => State.get[Int].at[Row])
    assertEquals(State.run(0)(runEither[Int, State % Int, String](p)), (5, Right(5)))

    val q = Reader.local[Int, Int, okay.Pure](_ * 10)(Reader.ask[Int])
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](5)(q)), 50)
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](5)(Reader.ask[Int].at[Reader % Int + okay.Pure])), 5)
  }

  // ---------------------------------------------------------- Reader.local

  test("local overrides asks inside p; outside it, ask sees the ambient value") {
    val p = Reader.local[Int, Int, okay.Pure](_ * 10)(Reader.ask[Int]).flatMap { inside =>
      Reader.ask[Int].at[Reader % Int + okay.Pure].map(outside => (inside, outside))
    }
    assertEquals(!.run(Reader.run[Int, (Int, Int), okay.Pure](5)(p)), (50, 5))
  }

  test("local composes across a flatMap chain inside p") {
    val p = Reader.local[Int, Int, okay.Pure](_ + 1)(
      Reader.ask[Int].flatMap(a => Reader.ask[Int].map(b => a + b)))
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](10)(p)), 22, "both asks inside p saw 11")
  }

  test("FOUND, NOT ASSUMED — nesting composes INSIDE-OUT: local(f2)(local(f1)(p)) sees f1(f2(r))") {
    // the naive mtl-style guess was f2(f1(r)) — the OUTER function
    // wrapped around the inner's result. That is wrong here, and the
    // mechanism is worth stating: `local`'s OWN "find the ambient r"
    // step is ITSELF an ordinary `Reader.ask` — ​indistinguishable, by
    // this row's own class-keyed typing, from any ask a USER writes.
    // So an ENCLOSING local's override reaches the INNER local's own
    // bookkeeping ask too, not only the user-visible one: the inner
    // local computes r2_inner = f1(f2(ambient)), and f1 ends up
    // applied LAST. mtl's `local` never has this problem because its
    // "current environment" is the INTERPRETER's own hidden state,
    // never a program-visible ask; here it is built FROM the same
    // vocabulary it overrides, and that is the whole difference.
    val p = Reader.local[Int, Int, okay.Pure](_ + 100)(
      Reader.local[Int, Int, okay.Pure](_ * 2)(Reader.ask[Int])
    ).flatMap { inner => Reader.ask[Int].at[Reader % Int + okay.Pure].map(outer => (inner, outer)) }
    assertEquals(!.run(Reader.run[Int, (Int, Int), okay.Pure](3)(p)), (206, 3),
      "(3+100)*2 = 206: the OUTER's +100 reached the inner's OWN ask for r, not just the user's")
  }

  test("local forwards other effects unchanged") {
    val p = Reader.local[Int, Int, State % String](_ + 1)(
      State.set[String]("touched").at[Reader % Int + State % String]
        .flatMap(_ => Reader.ask[Int].at[Reader % Int + State % String]))
    // the Reader.run below is not a discard: it is where local's OWN
    // "find r" ask (ambient=10) resolves, giving f(10)=11 to p's ask
    assertEquals(State.run("")(Reader.run[Int, Int, State % String](10)(p)),
      ("touched", 11), "State forwarded through local untouched")
  }

  test("A SECOND, MORE FUNDAMENTAL LIMIT — local does NOT reach inside an OPAQUE payload like Delim.push's body") {
    // the FIRST guess for this test claimed local composes through a
    // captured continuation's OWN re-invocations; measured, it does
    // not, and the reason is more basic than the multi-shot story:
    // `Delim.push(p)(body)` builds `Inject(Push(p, body))`, and
    // `body` is a PAYLOAD FIELD of that operation — not the Bind's
    // own attached continuation function. `Effects.handle`'s
    // forwarding arm re-wraps a Bind's OWN continuation (which is
    // why local DOES compose across an ordinary flatMap chain, and
    // WOULD compose through a captured continuation's re-invocations
    // IF that continuation were reached by walking an ordinary Bind —
    // the earlier tests in this file already show that half working).
    // It has no way to reach INTO a value carried as DATA inside
    // another effect's own operation, because `Effects.handle` is
    // generic over G and treats a G-operation as opaque, exactly as
    // it must to forward ANY effect without knowing its shape. Delim
    // is exactly the effect whose whole design puts programs (`body`,
    // the shift's own `f`) inside operation payloads — the one shape
    // a Bind-based generic handler cannot see through.
    val prompt = Delim.prompt[Int]
    type F = Reader % Int + okay.Pure
    val body: Int ! Delim + F =
      Delim.shift[Int, Int, F](prompt)(k => k(1).flatMap(a => k(2).map(b => a + b)))
        .flatMap(n => Reader.ask[Int].at[Delim + F].map(_ + n))
    val localized = Reader.local[Int, Int, Delim + okay.Pure](_ * 10)(
      Delim.push(prompt)(body).at[Reader % Int + (Delim + okay.Pure)])
    val driven: Int ! F =
      Delim.run[Int, F](localized.at[Delim + F])(using Delim.OneMachine.fresh[F])
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](1)(driven)), 5,
      "the *10 never reached body's asks at all: k(1)=1+1=2, k(2)=1+2=3, 2+3=5 — the TRUE ambient, unaffected by local")
  }

  test("THE DOCUMENTED TRAP: a Reader program VALUE carries no memory of where it was built") {
    // specs/scoped-effects-laws.md, Out of scope: a captured
    // continuation is, mechanically, exactly this — a Free-producing
    // closure with no lexical memory of any scope. Whoever hands it
    // to `local` decides what its asks answer, not wherever the value
    // was first written down. This is the whole mechanism behind the
    // Delim case too, demonstrated here without Delim's machinery.
    val k: Int ! Reader % Int = Reader.ask[Int] // built with NO local in sight
    val underLocal = Reader.local[Int, Int, okay.Pure](_ * 10)(k.at[Reader % Int + okay.Pure])
    assertEquals(!.run(Reader.run[Int, Int, okay.Pure](1)(underLocal)), 10,
      "k has no memory of its own textual origin; whoever wraps it decides")
    // and k ITSELF, run plain, still answers the ambient value —
    // it is a value, evaluated fresh each time it is handed to a runner
    assertEquals(!.run(Reader.run(1)(k)), 1)
  }
