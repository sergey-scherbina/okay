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
/** what the services answer */
object Api:
  case class User(id: Int, name: String, planId: Int, banned: Boolean)
  case class Plan(id: Int, expired: Boolean)

enum Response:
  case Status, Banned, Expired
  case Page(text: String)

/** the three remote lookups a page can need: the PROGRAM says what it
 * needs, the HANDLER calls out — so the handler is what records the
 * calls made */
enum Fetch[+A] derives okay.Effect:
  case User(token: String) extends Fetch[Api.User]
  case Plan(id: Int) extends Fetch[Api.Plan]
  case Feed(id: Int) extends Fetch[List[String]]

object Fetch:
  type Row = Fetch + Once

  /** the constructors carry the row, so a block needs no type argument
   * and no ascription: `effect(User(token))` on its own would infer
   * `Nothing ! Fetch` — no answer type, and no Once beside it */
  def user(token: String): Api.User ! Row = effect(User(token))
  def plan(id: Int): Api.Plan ! Row = effect(Plan(id))
  def feed(id: Int): List[String] ! Row = effect(Feed(id))

  private def answer(e: Fetch[Any]): Any = e match
    case User(t) => t match
      case "banned" => Api.User(1, "Ada", 7, banned = true)
      case "expired" => Api.User(2, "Bob", 8, banned = false)
      case _ => Api.User(3, "Cleo", 9, banned = false)
    case Plan(id) => Api.Plan(id, expired = id == 8)
    case Feed(_) => List("scala", "okay", "effects")

  private def name(e: Fetch[Any]): String = e match
    case User(_) => "GET /user"
    case Plan(_) => "GET /plan"
    case Feed(_) => "GET /feed"

  /** every call this program makes, in order */
  def calls[A](p: A ! Row): (Seq[String], A) =
    import okay.!.*
    @scala.annotation.tailrec
    def loop(acc: Vector[String], x: A ! Fetch): (Seq[String], A) = (x.resume: @unchecked) match
      case Free.Pure(a) => (acc, a)
      case Inject(e) => (acc :+ name(e), answer(e).asInstanceOf[A])
      case Bind(Inject(e), f) =>
        loop(acc :+ name(e), f.asInstanceOf[Any => A ! Fetch](answer(e)))
    loop(Vector.empty, Once.run(p))

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

  test("no marks, no ascriptions: val is by value, lazy val by need, def by name") {
    def fetch(key: String): Int ! R = direct { key.tell; key.length }
    def demo(use: Boolean): Int ! R = direct:
      val      x = fetch("val")
      lazy val y = fetch("lazy val")
      def      z = fetch("def")
      if use then x + x + y + y + z + z else 0
    assertEquals(logged(demo(false))._1, Seq("val"))
    assertEquals(logged(demo(true))._1, Seq("val", "lazy val", "def", "def"))
  }

  test("a colourless val of the block's program type still runs when nothing uses it") {
    def told(s: String): Int ! R = direct { s.tell; s.length }
    val prog: Int ! R = direct:
      @scala.annotation.unused val x = told("x")
      1
    assertEquals(logged(prog), (Seq("x"), 1))
  }

  test("a val held as a PROGRAM is untouched: it is a value, run where it is marked") {
    def told(s: String): Int ! R = direct { s.tell; s.length }
    val prog: Int ! R = direct:
      val p = told("p")
      !p + !p
    assertEquals(logged(prog), (Seq("p", "p"), 2))
  }

  test("a val used BOTH as a program and as a value is refused with both readings") {
    val e = compileErrors("""
      import okay.*, okay.Direct.*
      import scala.language.implicitConversions
      type R = Once + Writer % String
      def told(s: String): Int ! R = direct { s.tell; s.length }
      val prog: Int ! R = direct {
        val p = told("p")
        val n = p + 1
        !p + n
      }
    """)
    assert(e.contains("BOTH ways"), e)
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

  // ---- the realistic shape: one handler, one word each

  /**
   * Written top to bottom as if everything were already loaded. The
   * three words are Scala's own, and they mean here what they mean for
   * values: `user` is fetched on every request, `plan` only if a branch
   * reaches it and then once, `feed` once per mention — and the last
   * line mentions it twice.
   */
  def page(path: String, token: String): Response ! Fetch + Once = direct:
    val      user = Fetch.user(token)         // by value: always, once
    lazy val plan = Fetch.plan(user.planId)   // by need:  if reached, once
    def      feed = Fetch.feed(user.id)       // by name:  at every mention

    if path == "/status" then Response.Status
    else if user.banned then Response.Banned
    else if plan.expired then Response.Expired
    else Response.Page(s"${feed.size} picks for ${user.name}, top ${feed.head}")

  test("one handler, three words: the calls each request makes") {
    // a page nobody needs the user for — and `val` fetches them anyway
    assertEquals(Fetch.calls(page("/status", "x")),
      (Seq("GET /user"), Response.Status))
    // the branch stops before plan: by need, so no /plan call
    assertEquals(Fetch.calls(page("/feed", "banned")),
      (Seq("GET /user"), Response.Banned))
    // reached, and fetched once
    assertEquals(Fetch.calls(page("/feed", "expired")),
      (Seq("GET /user", "GET /plan"), Response.Expired))
    // feed is mentioned twice in the last line, and by name is twice
    assertEquals(Fetch.calls(page("/feed", "ok")),
      (Seq("GET /user", "GET /plan", "GET /feed", "GET /feed"),
        Response.Page("3 picks for Cleo, top scala")))
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
