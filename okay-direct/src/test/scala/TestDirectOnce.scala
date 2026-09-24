package okay

import okay.Row.*
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
  case class User(id: Int, name: String, banned: Boolean)

enum Response:
  case Banned(user: Api.User)
  case Page(text: String, tookMs: Long)

/** what a page can need: two lookups and the clock. The PROGRAM says
 * what it needs; a HANDLER answers, and the test's handler below is
 * the one that records the calls. */
enum Fetch[+A] derives okay.Effect:
  case User(token: String) extends Fetch[Api.User]
  case Feed(id: Int) extends Fetch[List[String]]
  case Time() extends Fetch[Long]

object Fetch:
  type Row = Fetch + Once

  /** the constructors carry the row, so a block needs no type argument
   * and no ascription: `effect(User(token))` on its own would infer
   * `Nothing ! Fetch` — no answer type, and no Once beside it */
  def user(token: String): Api.User ! Row = effect(User(token))
  def feed(id: Int): List[String] ! Row = effect(Feed(id))
  def time: Long ! Row = effect(Time())

  extension (e: Fetch[?]) def show: String = e match
    case User(t) => s"GET /user?token=$t"
    case Feed(i) => s"GET /feed/$i"
    case Time() => "CLOCK"

/** your data — each part read BY ITS TYPE, so a part may be added
 * without touching the reads of the others */
case class Users(byToken: Map[String, Api.User]):
  def get(token: String): Api.User = byToken(token)
case class Feeds(byUser: Map[Int, List[String]]):
  def get(id: Int): List[String] = byUser(id)

/** the application names its environment once */
def read[T](using Reader.Has[(Users, Feeds), T]): T ! Reader % (Users, Feeds) =
  Reader.read[(Users, Feeds), T]

/**
 * The test: your data goes IN through Reader, the calls come OUT
 * through Writer, and the clock moves because each call costs time
 * (State). No mocks and no doubles — another handler for the same
 * effect, and itself an ordinary `direct` block.
 */
type Test = Writer % String + Reader % (Users, Feeds) + State % Long

def test[X](e: Fetch[X]): X ! Test = direct:
  e.show.tell
  // `: X` since Free[F, +A] (free-answer-variance): the block's answer
  // no longer pins the match's type, so its first arm (`Long`) would
  // type it and the others fail. The macro cannot ascribe it for you —
  // the block is typed before the macro runs; TestDirectGadtTail pins
  // why (a dotty inference rule, reproduced with no macro at all)
  (e match
    // the one mark: a GADT branch whose value IS the match's answer
    // types at the abstract X, where colouring cannot reach
    case Fetch.Time() => !State.modify[Long](_ + 10)
    case Fetch.User(t) => read[Users].get(t)
    case Fetch.Feed(i) => read[Feeds].get(i)): X

object Runner:
  /** the calls a program made, and its answer */
  def calls[A](env: (Users, Feeds))(p: A ! Fetch.Row): (Seq[String], A) =
    val handled = !.translate[A, Fetch, Test + Once](p.at[Fetch + (Test + Once)]):
      [X] => (e: Fetch[X]) => test(e).plus[Once]
    State.run[Long, (Seq[String], A)](0L)(
      Reader.run(env)(Writer.run[String, A, Reader % (Users, Feeds) + State % Long](
        Once.run(handled))))._2

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

  test("lazy val with a mark: a demand inside a filter body forces the cell once (direct-loops v2)") {
    // `filter` used to be the canonical refused lambda; it is a loop
    // now, and the loop body is a statement of the block — so the
    // demand reaches the cell and forces it exactly once
    val prog: Int ! R = direct {
      lazy val x: Int = { Writer("x").reflect; 1 }
      List(1, 2, 3).filter(i => i > x).sum
    }
    assertEquals(logged(prog), (Seq("x"), 5))
  }

  test("lazy val with a mark: a demand under a lambda that is NOT a loop stays refused") {
    val e = compileErrors("""
      val p: Int ! R = direct {
        lazy val x: Int = { Writer("x").reflect; 1 }
        List(1, 2).sortBy(i => i - x).sum
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

  // ---- the realistic shape: one handler, one word each, each for a reason

  /**
   * A page that stamps its own duration. Every word here is chosen for
   * CORRECTNESS, not for speed:
   *
   *   `started` is by value — pin the start. As a `def` it would move
   *      with the end and the duration would always be 0.
   *   `user` is by value — every branch needs it.
   *   `feed` is by need — costly, only one branch reaches it, and its
   *      two reads (size, head) must see ONE list. As a `def` it would
   *      fetch twice and could report one list's size with another's head.
   *   `now` is by name — time MOVES, so each mention reads it again.
   *      As a `lazy val` the duration would always be 0.
   */
  def page(token: String): Response ! Fetch + Once = direct:
    val      started = Fetch.time          // by value: pin the start, once
    val      user    = Fetch.user(token)   // by value: every branch needs it
    lazy val feed    = Fetch.feed(user.id) // by need:  costly, and ONE list for both reads
    def      now     = Fetch.time          // by name:  time moves, read it again

    if user.banned then Response.Banned(user)
    else Response.Page(s"${feed.size} picks for ${user.name}, top ${feed.head}", now - started)

  private val env = (
    Users(Map("b" -> Api.User(1, "Ada", banned = true),
      "o" -> Api.User(3, "Cleo", banned = false))),
    Feeds(Map(1 -> Nil, 3 -> List("scala", "okay", "effects"))))

  test("one handler, three words: the calls each request makes") {
    // banned: the clock is pinned and read once, the user is fetched,
    // and feed is never reached — so it is never fetched
    assertEquals(Runner.calls(env)(page("b")),
      (Seq("CLOCK", "GET /user?token=b"), Response.Banned(Api.User(1, "Ada", banned = true))))

    // the full page: feed is READ TWICE (size, head) and fetched once —
    // by need. The clock is read twice and answers twice, 120ms apart —
    // by name. Four calls, at 40ms each.
    assertEquals(Runner.calls(env)(page("o")),
      (Seq("CLOCK", "GET /user?token=o", "GET /feed/3", "CLOCK"),
        Response.Page("3 picks for Cleo, top scala", 10L)))
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
