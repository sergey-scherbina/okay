package okay.freer
/**
 * specs/shift0-dollar.md, STAGE 0: what today's `Shift` already says
 * about shift0 and λ$'s dollar, before anything is built.
 *
 * λ$ (Materzok & Biernacki, APLAS 2012) has one delimiter, `v $ e`:
 * run `e` in a new context, and when it returns `x`, leave the
 * delimiter and call `v x`. Its contraction rules are
 *
 *     v' $ v      ⇝ v' v                   ($v)
 *     K̂[S0 f.e]   ⇝ e[λx.K̂[x]/f]          ($/S0)   K̂ = v $ K
 *
 * so the context a shift0 captures CONTAINS `v`. The paper also
 * macro-expresses `$` with reset0 and shift0:
 *
 *     e1 $ e2 = (λk. ⟨(λx. S0 z. k x) e2⟩) e1
 *
 * which is written below on today's `push`/`shift0` as `dollarMacro`.
 * The suite pins three facts: the ICFP 2011 example runs on today's
 * shift0, the macro obeys both rules, and the obvious encoding
 * `push(p)(e).map(v)` obeys `($v)` and NOT `($/S0)` — the difference
 * stage 1's primitive exists to remove.
 */
class TestDollarProbe extends munit.FunSuite:

  type Row = Shift % ? + Pure

  def run(p: String ! Row): String = !.run(Shift.run[String, Pure](p))

  def shift0(p: Prompt[String])(f: (String => String ! Row) => String ! Row): String ! Row =
    Shift.shift0[String, String, Pure](p)(f)

  /** APLAS 2012's macro-expression of `$`, on today's operators: run
   * `e` under the delimiter, then escape PAST it with `v` applied */
  def dollarMacro(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Shift.push(p)(e.flatMap(x => shift0(p)(_ => v(x))))

  /** the obvious encoding: `v` applied AFTER the delimiter, as a frame
   * a 0-capture does not take */
  def dollarFlatMap(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Shift.push(p)(e).flatMap(v)

  val angle: String => String ! Row = x => okay.freer.pure(s"<$x>")

  // ------------------------------------------------ ICFP 2011, section 1

  test("ICFP 2011: ⟨\"Alice\" ++ ⟨\" has \" ++ (S0 k1. S0 k2. \"A cat\" ++ k1 (k2 \".\"))⟩⟩ is \"A cat has Alice.\"") {
    val p = Shift.prompt[String]
    val inner = shift0(p)(k1 => shift0(p)(k2 => k2(".").flatMap(k1).map("A cat" + _)))
    val prog = Shift.push(p)(Shift.push(p)(inner.map(" has " + _)).map("Alice" + _))
    assertEquals(run(prog), "A cat has Alice.")
  }

  // ------------------------------------------------ ($v): both encodings agree

  test("($v): a body that returns a value — v applied once, in both encodings") {
    val p = Shift.prompt[String]
    assertEquals(run(dollarMacro(p)(angle)(okay.freer.pure("x"))), "<x>")
    val q = Shift.prompt[String]
    assertEquals(run(dollarFlatMap(q)(angle)(okay.freer.pure("x"))), "<x>")
  }

  // ------------------------------------------------ ($/S0): only the macro

  test("($/S0), k DROPPED: λ$ never applies v — the macro agrees, push.map does not") {
    val body: Prompt[String] => String ! Row = p => shift0(p)(_ => okay.freer.pure("dropped")).map(_ + "!")
    val p = Shift.prompt[String]
    assertEquals(run(dollarMacro(p)(angle)(body(p))), "dropped")
    val q = Shift.prompt[String]
    assertEquals(run(dollarFlatMap(q)(angle)(body(q))), "<dropped>", "v applied to the escape")
  }

  test("($/S0), k called TWICE: λ$ applies v per call — the macro agrees, push.map does not") {
    val body: Prompt[String] => String ! Row = p =>
      shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
    val p = Shift.prompt[String]
    assertEquals(run(dollarMacro(p)(angle)(body(p))), "<a!><b!>")
    val q = Shift.prompt[String]
    assertEquals(run(dollarFlatMap(q)(angle)(body(q))), "<a!b!>", "v applied once, to the concatenation")
  }

  // ------------------------------------------------ the spec's OPEN typing question

  test("shift under $: APLAS 2012 builds S k.e as S0 k.⟨e⟩ — the body runs under a PLAIN delimiter, k carries v") {
    // v $ E[S k. k a] = ⟨k a⟩ with k = λx. v $ E[x], so = v(E[a]).
    val p = Shift.prompt[String]
    val body = Shift.shift[String, String, Pure](p)(k => k("a").map(_ + "|body")).map(_ + "!")
    assertEquals(run(dollarMacro(p)(angle)(body)), "<a!>|body")
    // the same with the construction spelled out: S0 k. reset0 (body)
    val q = Shift.prompt[String]
    val viaS0 = shift0(q)(k => Shift.push(q)(k("a").map(_ + "|body"))).map(_ + "!")
    assertEquals(run(dollarMacro(q)(angle)(viaS0)), "<a!>|body")
  }
