package okay

import Layered.*

/**
 * specs/layered-reflection.md stage 0: several monads in one block,
 * each reflect reaching its own reify, on today's multi-prompt Delim.
 */
class TestLayered extends munit.FunSuite:

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

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

/** specs/layered-reflection.md stage 2: the stacked layers */
class TestLayeredStacked extends munit.FunSuite:
  import okay.Delim.Stacked.delimited
  import okay.Layered.Stacked.{reify, reflect}
  import okay.Prog.{flatMap, map}

  type P = okay.Pure

  test("stacked: List outside Option, each reflect reaching its own layer — the stage-0 answer") {
    val r = !.run(delimited[List[Option[Int]], P] { root =>
      import root.given
      reify[List, Option[Int], P] { lst =>
        import lst.given
        reify[Option, Int, P] { opt =>
          import opt.given
          for
            x <- List(1, 2, 3).reflect[Option[Int], P](lst)
            y <- (if x == 2 then None else Some(x * 10)).reflect[Int, P](opt)
          yield x + y
        }
      }
    })
    assertEquals(r, List(Some(11), None, Some(33)))
  }

  test("stacked: Option outside List — None empties everything") {
    val r = !.run(delimited[Option[List[Int]], P] { root =>
      import root.given
      reify[Option, List[Int], P] { opt =>
        import opt.given
        reify[List, Int, P] { lst =>
          import lst.given
          for
            x <- List(1, 2, 3).reflect[Int, P](lst)
            y <- (if x == 2 then None else Some(x * 10)).reflect[List[Int], P](opt)
          yield x + y
        }
      }
    })
    assertEquals(r, None)
  }

  test("stacked: a layer used AFTER its reify returned does not compile (stage 0 threw NoPrompt)") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[Option[Int], okay.Pure] { root =>
        import root.given
        var leaked: okay.Delim.Stacked.In[Option[Int], ?] | Null = null
        okay.Layered.Stacked.reify[Option, Int, okay.Pure] { opt =>
          import opt.given
          leaked = opt
          okay.Prog.pure(1)
        }.flatMap(_ => okay.Layered.Stacked.reflect(Option(2))[Int, okay.Pure](leaked.nn).map(Option(_)))
      }""")
    assert(e.contains("not on the prompt stack"), s"compiled, or not our message: $e")
  }
