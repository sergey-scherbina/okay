package okay

import okay.Row.plus
import okay.std.{Reader, Writer}

/** okay-freer's words over the root's program: a module on them moves by its imports alone */
class TestCompat extends munit.FunSuite:
  def countdown(n: Int): Int ! Writer % Int =
    if n == 0 then pure(0) else Writer.tell(n).flatMap(_ => !.tailcall(countdown(n - 1)))

  test("!.run, !.tailcall, .plus, !.widen, Row.at, Free.delay"):
    val p: Int ! Writer % Int + Reader % String = countdown(3).plus[Reader % String]
    val q: Int ! Writer % Int + Reader % String = !.widen[Int, Writer % Int, Reader % String](countdown(2))
    val r: Int ! Writer % Int + Reader % String = Free.delay(() => Reader.ask[String].map(_.length)).at[Writer % Int + Reader % String]
    def go(x: Int ! Writer % Int + Reader % String) = !.run(Reader.run("abcd")(Writer.run(x)))
    assertEquals(go(p)._1, Vector(3, 2, 1))
    assertEquals(go(q)._1, Vector(2, 1))
    assertEquals(go(r)._2, 4)

  test("!.loop: a million turns in constant stack"):
    assertEquals(!.run(!.loop[Int, Int, Pure](0)(i => pure(if i < 1000000 then Left(i + 1) else Right(i)))), 1000000)

  test("!.translate and Row.union"):
    val p: Int ! Reader % Int + Writer % Int = Reader.ask[Int].flatMap(i => Writer.tell(i).map(_ => i * 2))
    val t: Int ! Writer % Int = !.translate[Int, Reader % Int, Writer % Int](p)([X] => (op: Reader[Int, X]) => op match
      case Reader.Ask() => pure[X, Writer % Int](7.asInstanceOf[X])
      case other => throw MatchError(other))
    assertEquals(!.run(Writer.run(t)), Tuple2(Vector(7), 14))

  test("recover, ensure: the kernel's guards on a program"):
    var fin = 0
    val boom: Int ! Writer % Int = Writer.tell(1).map(_ => throw IllegalStateException("boom"))
    val p = boom.recover(e => Writer.tell(e.getMessage.length).map(_ => -1)).ensure(defer { fin += 1; pure(()) })
    assertEquals(!.run(Writer.run(p)), Tuple2(Vector(1, 4), -1))
    assertEquals(fin, 1)
