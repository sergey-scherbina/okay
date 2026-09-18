package okay

import okay.given

/**
 * A TYPESTATE PROGRAM IS A SEMIGROUPOID, NOT A CATEGORY — and the
 * reason is the same one that refuses it a `Choice` (cont-category,
 * 2026-09-18, the operator asking what optics and arrows do WITH
 * continuations).
 *
 * `PState.Zooming[X, R] = [A, B] =>> Cont[X, B => R, A => R]` — a
 * program computing an `X` that takes the state from `A` to `B`. Read
 * as `P[A, B]`:
 *
 *   - it IS `Strong`, so every lens zooms it (`PState.zoom` is that
 *     instance and nothing else);
 *   - `compose` is writable and means what it should: run one
 *     typestate program, then the next, threading the state A -> B -> C;
 *   - `id` IS NOT. The identity must compute an `X` while leaving the
 *     state alone, and `X` is universally quantified — only the inner
 *     program can make one. `idGiven` below is the honest version:
 *     hand it an `X` and it exists.
 *   - and `Choice.right` fails for the SAME reason one level along: on
 *     the absent case it must still answer an `X` and has no program
 *     to get one from.
 *
 * ONE CAUSE, TWO REFUSALS, which is what makes this worth a file. A
 * category without an identity is a SEMIGROUPOID, and that is exactly
 * what this is.
 */
class TestContSemigroupoid extends munit.FunSuite {

  private type Prog[X, R] = [A, B] =>> PState.Zooming[X, R][A, B]

  /** run one typestate program then the next — the semigroupoid's compose */
  private def andThenP[X, R, A, B, C](f: PState.Zooming[X, R][A, B],
                                      g: PState.Zooming[X, R][B, C]): PState.Zooming[X, R][A, C] =
    shift(k => (a: A) => (f / (x => (b: B) => (g / (_ => (c: C) => k(x)(c)))(b)))(a))

  /** the identity, WITH the answer handed in — the only way it exists */
  private def idGiven[X, R, A](x: X): PState.Zooming[X, R][A, A] =
    shift(k => (a: A) => k(x)(a))

  test("compose: two typestate programs, in order, with the state threaded") {
    // R is what the RUN answers — `(final state, value)` — so it is
    // `(Boolean, Int)` here and not `(String, Int)`: the middle state
    // is a waypoint, not the answer. The compiler caught that.
    type R = (Boolean, Int)
    // Int -> String, then String -> Boolean
    val toText: PState.Zooming[Int, R][Int, String] =
      PState.get[Int, R].flatMap(n => PState.set[Int, String, R](s"n=$n").map(_ => n))
    val toFlag: PState.Zooming[Int, R][String, Boolean] =
      PState.get[String, R].flatMap(s => PState.set[String, Boolean, R](s.nonEmpty).map(_ => s.length))

    // the ANSWER is the first program's, which is what `compose` here
    // decides and states: the second's answer is dropped
    val both = andThenP(toText, toFlag)
    assertEquals(PState.run[Int, Boolean, Int](7)(both), (true, 7))
  }

  test("the identity exists ONLY with an X handed in") {
    type R = (Int, String)
    val id0 = idGiven[String, R, Int]("answered")
    assertEquals(PState.run[Int, Int, String](3)(id0), (3, "answered"))
  }

  test("NO Category and NO Choice for this carrier — one cause, two refusals") {
    import scala.compiletime.testing.typeCheckErrors
    // a Category would need `id` with no X to make it from
    val cat = typeCheckErrors("summon[okay.Optic.Category[okay.PState.Zooming[Int, Int]]]")
    assert(cat.nonEmpty, "a Category resolved — the refutation is stale")
    val choice = typeCheckErrors("summon[okay.Optic.Choice[okay.PState.Zooming[Int, Int]]]")
    assert(choice.nonEmpty, "a Choice resolved — the refutation is stale")
    // and the control: Strong DOES resolve, in the same scope
    assertEquals(typeCheckErrors("summon[okay.Optic.Strong[okay.PState.Zooming[Int, Int]]]"), Nil)
  }
}
