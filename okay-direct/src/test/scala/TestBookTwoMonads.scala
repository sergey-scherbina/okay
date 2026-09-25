package okay

/**
 * THE BOOK'S CHAPTER 16b, COMPILED
 * (docs/continuations/16b-two-monads-at-once.md).
 *
 * Two monads in one computation: why they do not compose by
 * themselves, what a monad transformer is and costs, Filinski's
 * reflection with one prompt (which still needs the transformer), and
 * layered reflection over `$` and `shift0` (which does not).
 */
object TwoMonads:

  /** the one fallible step every program in the chapter uses */
  def lookup(x: Int): Option[Int] = if x == 2 then None else Some(x * 10)

  given Monad[List] with
    override def pure[A](a: A): List[A] = List(a)
    extension [A](m: List[A])
      override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)

  /** Option INSIDE any monad F: the transformer every library ships */
  final case class OptionT[F[_], A](run: F[Option[A]]):
    def flatMap[B](f: A => OptionT[F, B])(using F: Monad[F]): OptionT[F, B] =
      OptionT(run.flatMap {
        case Some(a) => f(a).run
        case None    => F.pure(None)
      })
    def map[B](f: A => B)(using F: Monad[F]): OptionT[F, B] =
      flatMap(a => OptionT(F.pure(Some(f(a)))))

  object OptionT:
    def lift[F[_], A](fa: F[A])(using F: Monad[F]): OptionT[F, A] =
      OptionT(fa.flatMap(a => F.pure(Some(a))))

    /** the Monad instance Filinski's reflect asks for */
    given [F[_]](using F: Monad[F]): Monad[[A] =>> OptionT[F, A]] with
      override def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      extension [A](m: OptionT[F, A])
        override def flatMap[B](f: A => OptionT[F, B]): OptionT[F, B] = m.flatMap(f)

  /** List INSIDE any monad F: the other order is a second transformer */
  final case class ListT[F[_], A](run: F[List[A]]):
    def flatMap[B](f: A => ListT[F, B])(using F: Monad[F]): ListT[F, B] =
      ListT(run.flatMap { as =>
        as.foldRight(F.pure(List.empty[B])) { (a, rest) =>
          f(a).run.flatMap(bs => rest.flatMap(cs => F.pure(bs ++ cs)))
        }
      })
    def map[B](f: A => B)(using F: Monad[F]): ListT[F, B] =
      flatMap(a => ListT(F.pure(List(f(a)))))

  object ListT:
    def lift[F[_], A](fa: F[A])(using F: Monad[F]): ListT[F, A] =
      ListT(fa.flatMap(a => F.pure(List(a))))

import TwoMonads.{*, given}

/** I-II: the problem, and the transformer road */
class TestBookTwoMonads extends munit.FunSuite:

  test("each monad alone is easy") {
    val opt = for { a <- lookup(1); b <- lookup(3) } yield a + b
    val lst = for { x <- List(1, 2); y <- List(10, 20) } yield x + y
    assertEquals(opt, Some(40))
    assertEquals(lst, List(11, 21, 12, 22))
  }

  test("both at once, by hand: the plumbing is yours") {
    val byHand: List[Option[Int]] =
      List(1, 2, 3).map { x =>
        lookup(x) match
          case Some(y) => Some(x + y)
          case None    => None
      }
    assertEquals(byHand, List(Some(11), None, Some(33)))
  }

  test("OptionT over List: one for-comprehension again, at the price of lift and a wrapper") {
    val viaT: OptionT[List, Int] =
      for
        x <- OptionT.lift(List(1, 2, 3))
        y <- OptionT(List(lookup(x)))
      yield x + y
    assertEquals(viaT.run, List(Some(11), None, Some(33)))
  }

  test("the other order is another transformer: ListT over Option, and one None empties everything") {
    val viaT: ListT[Option, Int] =
      for
        x <- ListT(Option(List(1, 2, 3)))
        y <- ListT.lift(lookup(x))
      yield x + y
    assertEquals(viaT.run, None)
    val noFailure: ListT[Option, Int] =
      for
        x <- ListT(Option(List(1, 3)))
        y <- ListT.lift(lookup(x))
      yield x + y
    assertEquals(noFailure.run, Some(List(11, 33)))
  }

/** III: Filinski 1994 — one prompt, so one monad per block: the transformer stays */
class TestBookTwoMonadsFilinski extends munit.FunSuite:
  import okay.Cont.Monadic.*

  test("reflection into OptionT[List]: direct style, but still the transformer and its lift") {
    val viaFilinski: OptionT[List, Int] =
      reify:
        for
          x <- OptionT.lift(List(1, 2, 3)).reflect
          y <- OptionT(List(lookup(x))).reflect
        yield x + y
    assertEquals(viaFilinski.run, List(Some(11), None, Some(33)))
  }

/** IV: layered reflection — a delimiter ($) per monad, reflect as shift0 */
class TestBookTwoMonadsLayered extends munit.FunSuite:
  import Layered.{reify, reflect}

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

  test("List outside Option, no transformer: each reflect reaches its own reify") {
    val listOfOptions = reify[List, Option[Int], Pure]:
      reify[Option, Int, Pure]:
        for
          x <- List(1, 2, 3).reflect[Option[Int], Pure]
          y <- lookup(x).reflect[Int, Pure]
        yield x + y
    assertEquals(run(listOfOptions), List(Some(11), None, Some(33)))
  }

  test("swap the two blocks and you swap the layers: Option outside List") {
    val optionOfList = reify[Option, List[Int], Pure]:
      reify[List, Int, Pure]:
        for
          x <- List(1, 2, 3).reflect[Int, Pure]
          y <- lookup(x).reflect[List[Int], Pure]
        yield x + y
    assertEquals(run(optionOfList), None)
  }

  test("in a direct block the mark finds the layer by the value's type") {
    import okay.Direct.*
    val marked = reify[List, Option[Int], Pure]:
      reify[Option, Int, Pure]:
        direct:
          val x = List(1, 2, 3).?
          val y = lookup(x).?
          x + y
    assertEquals(run(marked), List(Some(11), None, Some(33)))
  }
