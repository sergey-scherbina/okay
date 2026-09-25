package bookcats

import okay.{!, +, %, Delim, Pure, pure}

/**
 * THE BOOK'S CHAPTER 16b, one program three ways
 * (docs/continuations/16b-two-monads-at-once.md): a basket bought in
 * one of several shops (List: every choice), where a price may be
 * missing (Either) and every step is written to a log (Writer). Written
 * with cats' monad transformers, with layered monadic reflection
 * (Materzok & Biernacki's `$` and `shift0`, okay's `Layered`), and with
 * algebraic effects (okay's rows and handlers). A separate package, so
 * `cats` is cats and not okay.cats.
 */
object Shops:
  val prices: Map[String, Map[String, Int]] = Map(
    "north" -> Map("tea" -> 300, "cake" -> 500),
    "south" -> Map("tea" -> 250))

/** I. monad transformers: the stack is a type, every operation is lifted to its place in it */
object CatsBasket:
  import cats.data.{EitherT, WriterT}
  import cats.syntax.all.*
  import Shops.prices

  type Branches[A] = WriterT[List, Vector[String], A]
  type App[A]      = EitherT[Branches, String, A]

  def choose[A](xs: List[A]): App[A] =
    EitherT.liftF(WriterT.liftF(xs))

  def log(msg: String): App[Unit] =
    EitherT.liftF(WriterT.tell[List, Vector[String]](Vector(msg)))

  def price(shop: String, item: String): App[Int] =
    EitherT.fromOption[Branches](prices(shop).get(item), s"$shop has no $item")

  def basket(items: List[String]): App[Int] =
    for
      shop  <- choose(List("north", "south"))
      _     <- log(s"shop $shop")
      ps    <- items.traverse(item => price(shop, item))
      total  = ps.sum
      _     <- log(s"total $total")
    yield total

  def run(items: List[String]): List[(Vector[String], Either[String, Int])] =
    basket(items).value.run

/** I½. stacks do not compose with each other: three teams, the same effects, three stacks */
object CatsStacks:
  import cats.data.{EitherT, ReaderT, WriterT}
  import CatsBasket.App

  /** team B: the same three effects, the other order — the error OUTSIDE the log */
  type Checked[A] = EitherT[List, String, A]
  type Audited[A] = WriterT[Checked, Vector[String], A]

  def audit(msg: String): Audited[Unit] =
    WriterT.tell[Checked, Vector[String]](Vector(msg))

  def reject[A](why: String): Audited[A] =
    WriterT.liftF(EitherT.leftT[List, A](why))

  /** team C: team A's stack with a configuration on top */
  final case class Config(vat: Int)
  type Configured[A] = ReaderT[App, Config, A]

  /** team A's price, reused by team C: one more lift, in every helper */
  def priced(shop: String, item: String): Configured[Int] =
    ReaderT.liftF(CatsBasket.price(shop, item))

  /** team B's helpers, reused by team A: a conversion by hand — and it
   * cannot keep a log the other order never had */
  def reorder[A](fa: Audited[A]): App[A] =
    EitherT(WriterT(fa.run.value.map {
      case Right((log, a)) => (log, Right(a))
      case Left(e)         => (Vector.empty, Left(e))
    }))

/** II. layered monadic reflection: a delimiter ($) per monad, reflect as shift0, no transformer */
object LayeredBasket:
  import okay.Layered.{Layer, reify, reflect}
  import Shops.prices

  /** Writer as a plain monad of the user's: a value and what was written */
  final case class Logged[A](log: Vector[String], value: A)

  /** its layer: bind appends the log of the continuation's program */
  given Layer[Logged] with
    def pure[A](a: A): Logged[A] = Logged(Vector.empty, a)
    def bind[A, B, G[+_]](m: Logged[A])(k: A => Logged[B] ! G): Logged[B] ! G =
      k(m.value).map(next => Logged(m.log ++ next.log, next.value))

  type Checked[A] = Either[String, A]
  type Out        = Logged[Either[String, Int]]

  def price(shop: String, item: String): Either[String, Int] =
    prices(shop).get(item).toRight(s"$shop has no $item")

  def basket(items: List[String]): List[Out] ! Delim + Pure =
    reify[List, Out, Pure]:
      reify[Logged, Either[String, Int], Pure]:
        reify[Checked, Int, Pure]:
          for
            shop  <- List("north", "south").reflect[Out, Pure]
            _     <- Logged(Vector(s"shop $shop"), ()).reflect[Either[String, Int], Pure]
            ps    <- items.foldLeft(pure[Delim + Pure, List[Int]](Nil))((acc, item) =>
                       acc.flatMap(xs => price(shop, item).reflect[Int, Pure].map(xs :+ _)))
            total  = ps.sum
            _     <- Logged(Vector(s"total $total"), ()).reflect[Either[String, Int], Pure]
          yield total

  def run(items: List[String]): List[(Vector[String], Either[String, Int])] =
    !.run(Delim.run[List[Out], Pure](basket(items))).map(o => (o.log, o.value))

  /** the other order, by swapping blocks: the error layer outermost */
  def failFirst(items: List[String]): Either[String, List[Logged[Int]]] ! Delim + Pure =
    reify[Checked, List[Logged[Int]], Pure]:
      reify[List, Logged[Int], Pure]:
        reify[Logged, Int, Pure]:
          for
            shop  <- List("north", "south").reflect[Logged[Int], Pure]
            _     <- Logged(Vector(s"shop $shop"), ()).reflect[Int, Pure]
            ps    <- items.foldLeft(pure[Delim + Pure, List[Int]](Nil))((acc, item) =>
                       acc.flatMap(xs => price(shop, item).reflect[List[Logged[Int]], Pure].map(xs :+ _)))
          yield ps.sum

/** III. algebraic effects: operations in a row, their meaning given by handlers where the program runs */
object EffectsBasket:
  import okay.{Choose, Throws, Writer, choose, raise, runChoice, runEither}
  import okay.Row.at
  import okay.given
  import Shops.prices

  type Basket = Choose + Writer % String + Throws % String

  def price(shop: String, item: String): Int ! Basket =
    prices(shop).get(item) match
      case Some(p) => pure[Basket, Int](p)
      case None    => raise[String, Int](s"$shop has no $item").at[Basket]

  def basket(items: List[String]): Int ! Basket =
    for
      shop  <- choose("north", "south").at[Basket]
      _     <- Writer.tell(s"shop $shop").at[Basket]
      ps    <- items.foldLeft(pure[Basket, List[Int]](Nil))((acc, item) => acc.flatMap(xs => price(shop, item).map(xs :+ _)))
      total  = ps.sum
      _     <- Writer.tell(s"total $total").at[Basket]
    yield total

  /** errors handled first, then the log, then the choice: each branch keeps its own log and outcome */
  def run(items: List[String]): List[(Vector[String], Either[String, Int])] =
    val checked = runEither[Int, Choose + Writer % String, String](basket(items))
    val logged  = Writer.collect[String, Either[String, Int], Choose](checked)
    !.run(runChoice[(Vector[String], Either[String, Int]), Pure](logged)).toList

  /** written ONCE against its own effect, used in any row that has it, in any order */
  def audit(msg: String): Unit ! Writer % String =
    Writer.tell(msg)

  /** the same helper in a program with MORE effects (a configuration) */
  type Taxed = okay.Reader % Int + Basket

  def taxed(items: List[String]): Int ! Taxed =
    for
      vat   <- okay.Reader.ask[Int].at[Taxed]
      total <- basket(items).at[Taxed]
      _     <- audit(s"vat $vat%").at[Taxed]
    yield total * (100 + vat) / 100

  /** the SAME program, errors handled last: one missing price fails the whole basket */
  def failFirst(items: List[String]): Either[String, Seq[(Vector[String], Int)]] =
    val logged  = Writer.collect[String, Int, Choose + Throws % String](basket(items))
    val chosen  = runChoice[(Vector[String], Int), Throws % String](logged)
    !.run(runEither[Seq[(Vector[String], Int)], Pure, String](chosen))

class TestBookTwoMonadsCats extends munit.FunSuite:

  val expected = List(
    (Vector("shop north", "total 800"), Right(800)),
    (Vector("shop south"), Left("south has no cake")))

  test("cats transformers: every shop, its own log, its own outcome") {
    assertEquals(CatsBasket.run(List("tea", "cake")), expected)
  }

  test("layered reflection: the same answer, no transformer, no lift") {
    assertEquals(LayeredBasket.run(List("tea", "cake")), expected)
  }

  test("algebraic effects: the same answer, from handlers chosen where it runs") {
    assertEquals(EffectsBasket.run(List("tea", "cake")), expected)
  }

  test("STACKS DO NOT COMPOSE: team B's helper (other order) does not type in team A's stack") {
    val e = compileErrors("""
      val p: bookcats.CatsBasket.App[Int] =
        for
          _ <- bookcats.CatsStacks.audit("checked")
          t <- bookcats.CatsBasket.price("north", "tea")
        yield t
    """)
    assert(e.contains("Found:    cats.data.WriterT[bookcats.CatsStacks.Checked") && e.contains("Required: bookcats.CatsBasket.App[Int]"), e)
  }

  test("STACKS DO NOT COMPOSE: team A's helper does not type in team C's stack (one more layer) without another lift") {
    val e = compileErrors("""
      val p: bookcats.CatsStacks.Configured[Int] =
        for
          t <- bookcats.CatsBasket.price("north", "tea")
        yield t
    """)
    assert(e.contains("Found:"), e)
    assertEquals(CatsStacks.priced("north", "tea").run(CatsStacks.Config(20)).value.run, List((Vector.empty[String], Right(300))))
  }

  test("reordering by hand loses information: team B's log before an error cannot survive the conversion") {
    import cats.syntax.all.*
    val teamB: CatsStacks.Audited[Int] = CatsStacks.audit("checked") *> CatsStacks.reject[Int]("no stock")
    assertEquals(teamB.run.value, List(Left("no stock")))
    assertEquals(CatsStacks.reorder(teamB).value.run, List((Vector.empty[String], Left("no stock"))))
    // in team A's own order the same two steps keep the line
    val teamA: CatsBasket.App[Int] = CatsBasket.log("checked") *> cats.data.EitherT.leftT[CatsBasket.Branches, Int]("no stock")
    assertEquals(teamA.value.run, List((Vector("checked"), Left("no stock"))))
  }

  test("effects: the helper written once works in a bigger row and under either handler order") {
    import okay.{Choose, Throws, Writer, runChoice, runEither}
    import okay.given
    val withVat = okay.Reader.run[Int, Int, EffectsBasket.Basket](20)(EffectsBasket.taxed(List("tea")))
    val checked = runEither[Int, Choose + Writer % String, String](withVat)
    val logged  = Writer.collect[String, Either[String, Int], Choose](checked)
    assertEquals(!.run(runChoice[(Vector[String], Either[String, Int]), Pure](logged)).toList, List(
      (Vector("shop north", "total 300", "vat 20%"), Right(360)),
      (Vector("shop south", "total 250", "vat 20%"), Right(300))))
  }

  test("the other order: layered by swapping blocks, effects by swapping handlers — one missing price fails it all") {
    assertEquals(!.run(Delim.run[Either[String, List[LayeredBasket.Logged[Int]]], Pure](
      LayeredBasket.failFirst(List("tea", "cake")))), Left("south has no cake"))
    assertEquals(EffectsBasket.failFirst(List("tea", "cake")), Left("south has no cake"))
    assertEquals(EffectsBasket.failFirst(List("tea")),
      Right(Seq((Vector("shop north", "total 300"), 300), (Vector("shop south", "total 250"), 250))))
  }
