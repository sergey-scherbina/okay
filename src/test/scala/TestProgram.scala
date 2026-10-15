package okay

import okay.core.std.{Reader, Writer}
import okay.core.{%, Split}
import okay.one.handler.Handler as OneHandler

/** specs/freer-on-core.md: the platform's program as a value — okay-freer's words over the one monad */
class TestProgram extends munit.FunSuite:
  def reader[R](r: R): Handler[Reader % R, [A] =>> A] = new Handler[Reader % R, [A] =>> A]:
    def kernel[T <: okay.core.Row, A]: OneHandler[Reader % R, T, A, A] = Reader[R, T, A](r)
  def writer[W]: Handler[Writer % W, [A] =>> (List[W], A)] = new Handler[Writer % W, [A] =>> (List[W], A)]:
    def kernel[T <: okay.core.Row, A]: OneHandler[Writer % W, T, A, (List[W], A)] = Writer.collect[W, T, A]
  def ask[R]: R ! Reader % R = effect(Reader.Ask[R]())
  def tell[W](w: W): Unit ! Writer % W = effect(Writer.Tell(w))

  def prog: Int ! Reader % Int + Writer % String =
    for
      n <- ask[Int]
      _ <- tell(s"n=$n")
    yield n + 1

  test("a program a value: p.handle(h).handle(h2).run, the handlers in either order"):
    assertEquals(prog.handle(reader(41)).handle(writer[String]).run, (List("n=41"), 42))
    assertEquals(prog.handle(writer[String]).handle(reader(41)).run, (List("n=41"), 42))

  test("held in a val, handled later; the effects written in another order"):
    val p: Int ! Writer % String + Reader % Int = prog
    val q = p.handle(reader(1))
    assertEquals(q.handle(writer[String]).run, (List("n=1"), 2))

  test("a runner, its rest any: Split; a chain"):
    def runReader[R, A, Es[_]](r: R)(p: A ! Es)(using s: Split[Es, Reader % R]): A ! s.Rest = p.handle(reader(r))
    assertEquals(runReader(7)(prog).handle(writer[String]).run, (List("n=7"), 8))

  test("a loop of 100 000 binds in constant stack"):
    def loop(i: Int, acc: Int): Int ! Reader % Int + Writer % String =
      if i == 0 then pure(acc) else ask[Int].flatMap(x => tell("x").flatMap(_ => loop(i - 1, acc + x)))
    assertEquals(loop(100000, 0).handle(reader(1)).handle(writer[String]).run._2, 100000)

  test("the two forms meet: a value's facade form, the facade form a value"):
    val q: Int !! Reader % Int + Writer % String = prog.ctx
    assertEquals(Program.of(q).handle(reader(2)).handle(writer[String]).run, (List("n=2"), 3))
