package okay

import okay.Direct.{*, given}
import scala.language.implicitConversions

/**
 * The phases of the direct compiler, each asserted ON ITS OWN
 * (specs/direct-macro.md, "Structure"): a probe runs one phase over a
 * block and hands back its decision as data, so what is asserted here
 * is the decision — not the behaviour of the program it would have
 * shaped, which every other TestDirect* suite already covers.
 */
class TestDirectPhases extends munit.FunSuite {

  type Prog[A] = A ! okay.Pure
  type W[A] = A ! Writer % String
  type Asy[A] = A ! Async

  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)

  def other(n: Int): Long ! okay.Pure = Free.pure(n.toLong)

  // ---- defer: the pre-pass, before any bind. The pass looks for the
  // def the block is the body of (a self-call is a call to IT), and
  // walks up from the splice owner to find one: a block that sits in
  // a class body, as a test's does, has none and is left alone — so
  // each probe below is the body of a local def, as a direct block
  // that recurses always is.

  test("defer: a marked call at the program type is deferred by default") {
    def shape = DirectProbe.deferred[Prog, Long] { val x = !other(1); x + 1 }
    assert(shape.contains("delay"), shape)
  }

  test("defer: not under eagerCalls — unless it is the tail, which is deferred under both") {
    import Direct.eagerCalls.given
    def body = DirectProbe.deferred[Prog, Long] { val x = !other(1); x + 1 }
    assert(!body.contains("delay"), body)
    def tail = DirectProbe.deferred[Prog, Long] { val x = 1; !other(x) }
    assert(tail.contains("delay"), tail)
  }

  test("defer: a call already under !.tailcall is wrapped once, not twice") {
    def shape = DirectProbe.deferred[Prog, Long] { !(!.tailcall(other(1))) }
    assertEquals(shape.sliding("delay".length).count(_ == "delay"), 1, shape)
  }

  test("defer: a block with no enclosing def is left as written") {
    val shape = DirectProbe.deferred[Prog, Long] { val x = !other(1); x + 1 }
    assert(!shape.contains("delay"), shape)
  }

  // ---- marks: what the reader wrote

  test("marks: every spelling counts, and so does the colouring conversion") {
    assertEquals(DirectProbe.marks[Option, Int] { val x = Some(1).reflect; x + Some(2).!? }, 2)
    assertEquals(DirectProbe.marks[Option, Int] { val x: Int = Some(1); x }, 1)
    assertEquals(DirectProbe.marks[Option, Int] { 1 + 1 }, 0)
  }

  // ---- row: what F is

  test("row: what a bare statement can RUN at") {
    assert(DirectProbe.runnable[W, Unit ! Writer % String].exists(_.endsWith("Unit")))
    assert(DirectProbe.runnable[W, Writer[String, Unit]].exists(_.endsWith("Unit")))
    assertEquals(DirectProbe.runnable[W, Option[Int]], None)
    assert(DirectProbe.runnable[Option, Option[Int]].exists(_.endsWith("Int")))
  }

  test("row: what a statement would silently drop — the error's own predicate") {
    assert(DirectProbe.dropped[W, Int ! Writer % String])   // a program
    assert(DirectProbe.dropped[Option, Option[Int]])           // the block's own F
    assert(!DirectProbe.dropped[W, Option[Int]])               // an unregistered carrier
    assert(!DirectProbe.dropped[W, Int])                       // a plain value
  }

  test("row: the drop predicate asks for the auto-colouring marker, Direct.Effect — not okay.Effect") {
    // a carrier registered ONLY as the marker (the opt-in the docs name)
    // is a drop; the same name in package okay is the narrower
    // okay.Effect, which this given does not satisfy — the difference
    // the move out of `object Direct` silently made, and this caught
    // `@unused`: the use is the macro's own Implicits.search, which
    // the lint cannot see (a compile-time use leaves no reference)
    @scala.annotation.unused given Direct.Effect[Option] with {}
    assert(DirectProbe.dropped[W, Option[Int]])
  }

  // ---- parallel: the leading independent run

  test("parallel: the run ends at the first leaf that mentions an earlier one") {
    assertEquals(
      DirectProbe.independentRun[Asy, Int] {
        val a = async(1).reflect
        val b = async(2).reflect
        val c = async(a).reflect
        a + b + c
      }, List("a", "b"))
    assertEquals(
      DirectProbe.independentRun[Asy, Int] {
        val a = async(1).reflect
        val b = async(a).reflect
        a + b
      }, List("a"))
    assertEquals(
      DirectProbe.independentRun[Asy, Int] {
        val n = 1
        val a = async(n).reflect
        a
      }, Nil)
  }

  // ---- core: the value slots of an application spine

  test("core: receiver and arguments are slots; a varargs contributes its elements") {
    def plus(a: Int, b: Int): Int = a + b
    val xs = List(1, 2, 3)
    val args = DirectProbe.slots[Option, Int] { plus(1 + 1, Some(2).reflect) }
    assertEquals(args.length, 2, args)
    assert(args(1).contains("reflect"), args)
    val recv = DirectProbe.slots[Option, List[Int]] { xs.take(Some(1).reflect) }
    assertEquals(recv.length, 2, recv)
    assert(recv.head.contains("xs"), recv)
    val interp = DirectProbe.slots[Option, String] { s"a${Some(1).reflect}b" }
    assertEquals(interp.length, 2, interp)
    assert(interp.last.contains("reflect"), interp)
  }
}
