package okay

import okay.Direct.*
import scala.language.implicitConversions

/**
 * Call-by-need for programs (direct-once, specs/direct-macro.md):
 * `Once` is the effect, `!.once(p)` the word, `Once.run` the handler
 * whose state is the cells. `Delay` is by-name (forced every time the
 * loop reaches it); a once'd program runs at the first demand and
 * answers from its cell after. In a `direct` block `lazy val x = !p`
 * is that word: the effect runs in the position of the FIRST demand,
 * once. Multi-shot is handler order: `runChoice(Once.run(p))`
 * backtracks the cells with the search, `Once.run(runChoice(p))`
 * shares one store across the branches.
 */
class TestDirectOnce extends munit.FunSuite {

  type W = Writer % String
  type R = Once + W

  def logged[A](p: A ! R): (Seq[String], A) =
    !.run(Writer.run[String, A, okay.Pure](Once.run(p)))

  // ---- the effect, without direct

  test("!.once: three demands, one run, one value") {
    var hits = 0
    val q: Int ! R = !.once(Free.delay(() => { hits += 1; pure(hits * 10) }))
    val prog: Int ! R = q.flatMap(a => q.flatMap(b => q.map(c => a + b + c)))
    assertEquals(logged(prog), (Seq(), 30))
    assertEquals(hits, 1)
  }

  test("!.once: never demanded, never run — construction does no work") {
    var hits = 0
    val q: Int ! R = !.once(Free.delay(() => { hits += 1; pure(1) }))
    assertEquals(logged(pure[R, Int](7).map(_ + 1)), (Seq(), 8))
    assertEquals(hits, 0)
    val _ = q
  }

  test("!.once: what is in it is exactly what was passed — a bare p beside it runs every time") {
    val p: Int ! R = direct { Writer("p").reflect; 1 }
    val q: Int ! R = !.once(p)
    val r: Int ! R = direct { !q + !p + !q + !p }
    assertEquals(logged(r), (Seq("p", "p", "p"), 4))
  }

  test("!.once: two calls are two handles") {
    val p: Int ! R = direct { Writer("p").reflect; 1 }
    val r: Int ! R = direct { !(!.once(p)) + !(!.once(p)) }
    assertEquals(logged(r), (Seq("p", "p"), 2))
  }

  test("!.once: a knot is a loud error, not a hang") {
    lazy val h: Int ! R = !.once(h.map(_ + 1))
    val e = intercept[IllegalStateException](logged(h))
    assert(e.getMessage.contains("Once"), e.getMessage)
  }

  test("!.once: the same program run twice replays the same trace — the tree holds no cell") {
    var hits = 0
    val q: Int ! R = !.once(Free.delay(() => { hits += 1; pure(hits) }))
    val prog: Int ! R = direct { !q + !q }
    assertEquals(logged(prog), (Seq(), 2))
    assertEquals(logged(prog), (Seq(), 4))
    assertEquals(hits, 2)
  }

  // ---- the sugar

  test("lazy val with a mark: the effect runs at the first demand, once") {
    val prog: Int ! R = direct:
      Writer("before").reflect
      lazy val x: Int = { Writer("x").reflect; 21 }
      Writer("after").reflect
      x + x
    assertEquals(logged(prog), (Seq("before", "after", "x"), 42))
  }

  test("lazy val with a mark: never demanded, never run") {
    val prog: Int ! R = direct:
      @scala.annotation.unused lazy val x: Int = { Writer("x").reflect; 21 }
      Writer("only").reflect
      1
    assertEquals(logged(prog), (Seq("only"), 1))
  }

  test("lazy val with a mark: demanded in one branch only") {
    def prog(b: Boolean): Int ! R = direct:
      lazy val x: Int = { Writer("x").reflect; 21 }
      if b then x else 0
    assertEquals(logged(prog(true)), (Seq("x"), 21))
    assertEquals(logged(prog(false)), (Seq(), 0))
  }

  test("lazy val with a mark: the prefix spelling, a program-typed rhs, and the three words side by side") {
    def told(s: String): Int ! R = direct { Writer(s).reflect; s.length }
    val prog: Int ! R = direct:
      lazy val x = !told("abc")     // runs at first use
      val y = !told("de")           // runs here
      x + x + y + !told("f")        // x once, y already, the bare mark now
    assertEquals(logged(prog), (Seq("de", "abc", "f"), 9))
  }

  test("lazy val whose rhs runs an operation by do-notation, no mark: still by-need") {
    val prog: Int ! R = direct:
      lazy val x = { Writer("x"): Unit; 3 }   // `: Unit` as TestDirect does: quiets E176, the macro strips it
      Writer("first").reflect
      x + x
    assertEquals(logged(prog), (Seq("first", "x"), 6))
  }

  test("lazy val with a pure rhs stays a plain lazy val") {
    var built = 0
    val prog: Int ! R = direct:
      lazy val x = { built += 1; 3 }
      Writer("only").reflect
      x + x
    assertEquals(logged(prog), (Seq("only"), 6))
    assertEquals(built, 1)
  }

  test("a lazy val whose rhs uses another lazy val: loaded in order, once each, only if demanded") {
    case class User(id: Int, planId: Int)
    def loadUser(id: Int): User ! R = direct { s"user $id".tell; User(id, id % 2) }
    def loadPlan(id: Int): Int ! R = direct { s"plan $id".tell; id * 10 }
    def handle(userId: Int): String ! R = direct:
      lazy val user = !loadUser(userId)
      lazy val plan = !loadPlan(user.planId)
      if userId == 0 then "guest" else s"hello ${user.id} on plan ${plan}, quota ${plan}"
    assertEquals(logged(handle(0)), (Seq(), "guest"))
    assertEquals(logged(handle(7)), (Seq("user 7", "plan 1"), "hello 7 on plan 10, quota 10"))
  }

  test("lazy val with a mark inside a loop body: a fresh cell per iteration") {
    val prog: Int ! R = direct:
      var acc = 0
      for i <- List(1, 2, 3) do
        lazy val x: Int = { Writer(s"x$i").reflect; i }
        acc += x + x
      acc
    assertEquals(logged(prog), (Seq("x1", "x2", "x3"), 12))
  }

  test("lazy val with a mark: a row without Once is refused, and the effect is named") {
    val e = compileErrors("""
      val p: Int ! W = direct {
        lazy val x: Int = { Writer("x").reflect; 1 }
        x + x
      }
    """)
    assert(e.contains("Once"), e)
  }

  test("lazy val with a mark: a demand under a lambda stays refused") {
    val e = compileErrors("""
      val p: Int ! R = direct {
        lazy val x: Int = { Writer("x").reflect; 1 }
        List(1, 2).filter(i => i > x).sum
      }
    """)
    assert(e.contains("lambda"), e)
  }

  test("lazy val with a mark: demanded inside a for-yield, the loop the macro owns — once, at the first element") {
    val prog: List[Int] ! R = direct:
      lazy val x: Int = { Writer("x").reflect; 10 }
      for i <- List(1, 2, 3) yield i + x
    assertEquals(logged(prog), (Seq("x"), List(11, 12, 13)))
  }

  test("lazy val with a mark that names itself is refused") {
    val e = compileErrors("""
      val p: Int ! R = direct {
        lazy val x: Int = { Writer("x").reflect; x + 1 }
        x
      }
    """)
    assert(e.contains("itself"), e)
  }

  test("lazy val with a mark in a block whose monad is not a program stays refused") {
    val e = compileErrors(
      "okay.Direct.direct[Option] { lazy val x: Int = Option(1).reflect; x + x }")
    assert(e.contains("lazy val"), e)
  }

  // ---- multi-shot is handler order

  var hitsInside = 0
  val insideProg: Int ! (Once + (Choose + W)) = direct:
    lazy val x: Int = { Writer("x").reflect; hitsInside += 1; 10 }
    val b = !Choose(Seq(1, 2))
    b + x

  test("Once.run INSIDE the search: the cells backtrack, each branch runs its own once") {
    val handled: Seq[Int] ! W = runChoice[Int, W](Once.run[Int, Choose + W](insideProg))
    val (log, out) = !.run(Writer.run[String, Seq[Int], okay.Pure](handled))
    assertEquals(out, Seq(11, 12))
    assertEquals(hitsInside, 2)
    assertEquals(log, Seq("x", "x"))
  }

  var hitsOutside = 0
  val outsideProg: Int ! (Choose + (Once + W)) = direct:
    lazy val x: Int = { Writer("x").reflect; hitsOutside += 1; 10 }
    val b = !Choose(Seq(1, 2))
    b + x

  test("Once.run OUTSIDE the search: one store, the second branch sees the first's value") {
    val handled: Seq[Int] ! W = Once.run[Seq[Int], W](runChoice[Int, Once + W](outsideProg))
    val (log, out) = !.run(Writer.run[String, Seq[Int], okay.Pure](handled))
    assertEquals(out, Seq(11, 12))
    assertEquals(hitsOutside, 1)
    assertEquals(log, Seq("x"))
  }
}
