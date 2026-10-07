package okay.freer
import Layered.*

/**
 * specs/layered-reflection.md stage 0: several monads in one block,
 * each reflect reaching its own reify, on today's multi-prompt Shift.
 */
class TestLayered extends munit.FunSuite:

  def run[A](p: A ! Shift % ? + Pure): A = !.run(Shift.run[A, Pure](p))

  // ------------------------------------------------ one layer: Filinski's laws

  test("reify (reflect m) = m, for Option and List") {
    for m <- List(Some(3), None) do
      assertEquals(run(reify[Option, Int, Pure](m.reflect[Int, Pure])), m)
    for m <- List(List(1, 2, 3), Nil) do
      assertEquals(run(reify[List, Int, Pure](m.reflect[Int, Pure])), m)
  }

  test("one layer in direct style: Option short-circuits, List branches") {
    val opt = reify[Option, Int, Pure]:
      for
        a <- Option(2).reflect[Int, Pure]
        b <- Option(5).reflect[Int, Pure]
      yield a * b
    assertEquals(run(opt), Some(10))
    val none = reify[Option, Int, Pure]:
      for
        a <- Option(2).reflect[Int, Pure]
        b <- (None: Option[Int]).reflect[Int, Pure]
      yield a * b
    assertEquals(run(none), None)
    val pairs = reify[List, (Int, Char), Pure]:
      for
        n <- List(1, 2).reflect[(Int, Char), Pure]
        c <- List('a', 'b').reflect[(Int, Char), Pure]
      yield (n, c)
    assertEquals(run(pairs), List((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')))
  }

  // ------------------------------------------------ two layers in one block

  test("TWO LAYERS, Option outside List: one None empties everything — Option[List[Int]]") {
    val prog = reify[Option, List[Int], Pure]:
      reify[List, Int, Pure]:
        for
          x <- List(1, 2, 3).reflect[Int, Pure]
          y <- (if x == 2 then None else Some(x * 10)).reflect[List[Int], Pure]
        yield x + y
    assertEquals(run(prog), None)
  }

  test("TWO LAYERS, List outside Option: a None per failing branch — List[Option[Int]]") {
    val prog = reify[List, Option[Int], Pure]:
      reify[Option, Int, Pure]:
        for
          x <- List(1, 2, 3).reflect[Option[Int], Pure]
          y <- (if x == 2 then None else Some(x * 10)).reflect[Int, Pure]
        yield x + y
    assertEquals(run(prog), List(Some(11), None, Some(33)))
  }

  test("the same order without a failure: both layers answer every branch") {
    val prog = reify[Option, List[Int], Pure]:
      reify[List, Int, Pure]:
        for
          x <- List(1, 2, 3).reflect[Int, Pure]
          y <- Option(x * 10).reflect[List[Int], Pure]
        yield x + y
    assertEquals(run(prog), Some(List(11, 22, 33)))
  }

  test("THREE LAYERS: Either outside List outside Option, each reflect reaching its own reify") {
    val prog = reify[[A] =>> Either[String, A], List[Option[Int]], Pure]:
      reify[List, Option[Int], Pure]:
        reify[Option, Int, Pure]:
          for
            x <- List(1, 2, 3, 4).reflect[Option[Int], Pure]
            _ <- (if x == 4 then Left("four") else Right(())).reflect[List[Option[Int]], Pure]
            y <- (if x == 2 then None else Some(x)).reflect[Int, Pure]
          yield y
    assertEquals(run(prog), Left("four"))
    val ok = reify[[A] =>> Either[String, A], List[Option[Int]], Pure]:
      reify[List, Option[Int], Pure]:
        reify[Option, Int, Pure]:
          for
            x <- List(1, 2, 3).reflect[Option[Int], Pure]
            _ <- (if x == 4 then Left("four") else Right(())).reflect[List[Option[Int]], Pure]
            y <- (if x == 2 then None else Some(x)).reflect[Int, Pure]
          yield y
    assertEquals(run(ok), Right(List(Some(1), None, Some(3))))
  }

  // ------------------------------------------------ the capability's scope

  test("a capability used OUTSIDE its reify fails loudly (NoPrompt) — stage 2 makes it a compile error") {
    var leaked: Option[Reflect[Option, Int]] = None
    val first = reify[Option, Int, Pure]:
      leaked = Some(summon[Reflect[Option, Int]])
      Option(1).reflect[Int, Pure]
    assertEquals(run(first), Some(1))
    val after = reify[List, Int, Pure]:
      given Reflect[Option, Int] = leaked.get
      Option(2).reflect[Int, Pure]
    intercept[NoPrompt](run(after))
  }

/** specs/layered-reflection.md stage 2: the keyed layers (shift-prompt-key: each layer's prompt a key in the row) */
class TestLayeredStacked extends munit.FunSuite:
  import okay.freer.Layered.Stacked.{reify, reflect}
  import okay.freer.Row.at

  type P = Pure

  test("keyed: List outside Option, each reflect reaching its own layer — the stage-0 answer") {
    val r = !.run(reify[List, Option[Int], P] { lst =>
      reify[Option, Int, Shift % lst.type + P] { opt =>
        for
          x <- List(1, 2, 3).reflect(lst).at[Shift % opt.type + Shift % lst.type + P]
          y <- (if x == 2 then None else Some(x * 10)).reflect(opt)
        yield x + y
      }
    })
    assertEquals(r, List(Some(11), None, Some(33)))
  }

  test("keyed: Option outside List — None empties everything") {
    val r = !.run(reify[Option, List[Int], P] { opt =>
      reify[List, Int, Shift % opt.type + P] { lst =>
        for
          x <- List(1, 2, 3).reflect(lst)
          y <- (if x == 2 then None else Some(x * 10)).reflect(opt).at[Shift % lst.type + Shift % opt.type + P]
        yield x + y
      }
    })
    assertEquals(r, None)
  }

  test("keyed: a layer used AFTER its reify returned does not compile where it is run (stage 0 threw NoPrompt)") {
    val e = compileErrors("""
      var leaked: okay.freer.Shift.Stacked.Reset[Option[Int], Pure] | Null = null
      okay.freer.!.run(okay.freer.Layered.Stacked.reify[Option, Int, Pure] { opt =>
        leaked = opt
        okay.freer.pure[okay.freer.Shift % opt.type + Pure, Int](1)
      }.flatMap { _ =>
        val l = leaked.nn
        okay.freer.Layered.Stacked.reflect(Option(2))(l).map(Option(_))
      })""")
    assert(e.replaceAll("\\s+", " ").contains("% (l :"), s"compiled, or not naming the escaped key: $e")
  }
