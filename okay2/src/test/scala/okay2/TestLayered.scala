package okay2

import Layered._

/**
 * okay2-layered: the twin of the Scala 3 core's TestLayered
 * (specs/layered-reflection.md stage 0), every expected value the same.
 * A body RECEIVES its capability here (no context functions in Scala 2).
 */
class TestLayered extends munit.FunSuite {

  type P = Pure
  type DP = Delim + P
  type EitherS[A] = Either[String, A]

  def run[A](p: A ! DP): A = !.run(Delim.run[A, P](p))

  // ------------------------------------------------ one layer: Filinski's laws

  test("reify (reflect m) = m, for Option and List") {
    for (m <- List(Some(3), None)) assertEquals(run(reify[Option, Int, P](l => m.reflect[Int, P](l))), m)
    for (m <- List(List(1, 2, 3), Nil)) assertEquals(run(reify[List, Int, P](l => m.reflect[Int, P](l))), m)
  }

  test("one layer in direct style: Option short-circuits, List branches") {
    val opt = reify[Option, Int, P] { l =>
      for {
        a <- Option(2).reflect[Int, P](l)
        b <- Option(5).reflect[Int, P](l)
      } yield a * b
    }
    assertEquals(run(opt), Some(10))
    val none = reify[Option, Int, P] { l =>
      for {
        a <- Option(2).reflect[Int, P](l)
        b <- (None: Option[Int]).reflect[Int, P](l)
      } yield a * b
    }
    assertEquals(run(none), None)
    val pairs = reify[List, (Int, Char), P] { l =>
      for {
        n <- List(1, 2).reflect[(Int, Char), P](l)
        c <- List('a', 'b').reflect[(Int, Char), P](l)
      } yield (n, c)
    }
    assertEquals(run(pairs), List((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')))
  }

  // ------------------------------------------------ two layers in one block

  test("TWO LAYERS, Option outside List: one None empties everything — Option[List[Int]]") {
    val prog = reify[Option, List[Int], P] { opt =>
      reify[List, Int, P] { lst =>
        for {
          x <- List(1, 2, 3).reflect[Int, P](lst)
          y <- (if (x == 2) None else Some(x * 10)).reflect[List[Int], P](opt)
        } yield x + y
      }
    }
    assertEquals(run(prog), None)
  }

  test("TWO LAYERS, List outside Option: a None per failing branch — List[Option[Int]]") {
    val prog = reify[List, Option[Int], P] { lst =>
      reify[Option, Int, P] { opt =>
        for {
          x <- List(1, 2, 3).reflect[Option[Int], P](lst)
          y <- (if (x == 2) None else Some(x * 10)).reflect[Int, P](opt)
        } yield x + y
      }
    }
    assertEquals(run(prog), List(Some(11), None, Some(33)))
  }

  test("the same order without a failure: both layers answer every branch") {
    val prog = reify[Option, List[Int], P] { opt =>
      reify[List, Int, P] { lst =>
        for {
          x <- List(1, 2, 3).reflect[Int, P](lst)
          y <- Option(x * 10).reflect[List[Int], P](opt)
        } yield x + y
      }
    }
    assertEquals(run(prog), Some(List(11, 22, 33)))
  }

  test("THREE LAYERS: Either outside List outside Option, each reflect reaching its own reify") {
    def prog(xs: List[Int]): EitherS[List[Option[Int]]] ! DP =
      reify[EitherS, List[Option[Int]], P] { eth =>
        reify[List, Option[Int], P] { lst =>
          reify[Option, Int, P] { opt =>
            for {
              x <- xs.reflect[Option[Int], P](lst)
              _ <- ((if (x == 4) Left("four") else Right(())): EitherS[Unit]).reflect[List[Option[Int]], P](eth)
              y <- (if (x == 2) None else Some(x)).reflect[Int, P](opt)
            } yield y
          }
        }
      }
    assertEquals(run(prog(List(1, 2, 3, 4))), Left("four"))
    assertEquals(run(prog(List(1, 2, 3))), Right(List(Some(1), None, Some(3))))
  }

  // ------------------------------------------------ the capability's scope

  test("a capability used OUTSIDE its reify fails loudly (NoPrompt)") {
    var leaked: Option[Reflect[Option, Int]] = None
    val first = reify[Option, Int, P] { l =>
      leaked = Some(l)
      Option(1).reflect[Int, P](l)
    }
    assertEquals(run(first), Some(1))
    val after = reify[List, Int, P](_ => Option(2).reflect[Int, P](leaked.get))
    intercept[NoPrompt](run(after))
  }
}
