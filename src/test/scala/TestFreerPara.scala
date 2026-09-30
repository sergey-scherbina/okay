package okay

import okay.Freer.{Return, Inject, Bind}

/**
 * `Freer` as Atkey's parameterised monad (freer-paramonad, 2026-09-30),
 * and the operator's question beside it, answered by compiling: an
 * EFFECT whose signature carries the indexes — typestate as DATA, a
 * `Get`/`Put` enum indexed by the state's type, not `PState`'s shift
 * bodies — under which reading of `S`/`R` does a handler for it type
 * on this base?
 *
 * Two readings of `Freer[G, S, R, A]`, and the base admits ONE:
 *
 *  - ANSWER TYPES (Cont's, PState's): a leaf is `(X => S) => R`, `R`
 *    is what the handler PRODUCES. A handler for an indexed signature
 *    is an indexed natural transformation `G ~> Shift` — it chooses
 *    the shift body, which is the one sentence specs/freer-base.md
 *    carries ("Free is Cont whose shift body the handler chooses") —
 *    and the runner is Cont's, typed by the GADT. `PSt` below is
 *    `PState` as data, run exactly so.
 *  - BEFORE/AFTER STATE (McBride's `IxFree`): `R` is the state the
 *    program CONSUMES, `S` the one it leaves. A handler threading a
 *    value of `R` cannot type the arms that READ it: matching `Return`
 *    on a base covariant in `R` says `S <: R` where the arm needs an
 *    `S` from an `R`, and matching `Get` binds the continuation's
 *    argument as a supertype of the case's state rather than as `R`.
 *    The variance `+R` came for `tailShift` and is the continuation's
 *    — a consumed index is the wrong way round. Pinned by
 *    `compileErrors`, so the next base change re-asks it.
 */
class TestFreerPara extends munit.FunSuite:

  // ------------------------------------------------ the instance itself

  /** a program against an ABSTRACT paramonad: the indexes compose end
   * to end and `pure` sits on the diagonal */
  def chain[M[_, _, _]](using P: ParaMonad[M])[A, B, C, S, T, R]
                       (m: M[A, S, R])(f: A => M[B, T, S])(g: B => M[C, T, T]): M[C, T, R] =
    P.flatMap(m)(a => P.flatMap(f(a))(b => P.flatMap(g(b))(c => P.pure(c))))

  test("Freer.Para[G] is a ParaMonad for any G: found, and its nodes are the tree's") {
    val P = summon[ParaMonad[Freer.Para[Freer.Lift[Pure]]]]
    val p: Freer[Freer.Lift[Pure], Unit, Unit, Int] = P.flatMap(P.pure[Int, Unit](1))(x => P.pure(x + 1))
    p match
      case Bind(Return(1), _) => ()
      case other => fail(s"expected Bind(Return(1), _), got $other")
    assertEquals(!.run(p.map(_ * 10)), 20)
    // map leaves the readable node, not a flatMap into a pure
    P.map(P.pure[Int, Unit](1))(_ + 1) match
      case Bind(_, f) => assert(f.isInstanceOf[Freer.Mapped[?, ?, ?, ?]])
      case other => fail(s"expected a Mapped bind, got $other")
  }

  test("the diagonal at Lift is Free's own Monad — no ambiguity with the bridge") {
    // both instances exist; the effect row's Monad still resolves and runs
    def sum[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(x + _))
    assertEquals(!.run(sum[[A] =>> A ! Pure](pure(1), pure(2))), 3)
  }

  // --------------------------- reading 1: the index is an ANSWER TYPE

  /**
   * `PState` AS DATA: the same signatures `PState.get`/`set` have
   * (State.scala), on an enum a handler can look at. `Get` keeps the
   * state's type, `Put` moves it from `S` to `T`; `Z` is the final
   * answer, threaded as `PState` threads it.
   */
  enum PSt[S, +R, +X]:
    case Get[S, Z]() extends PSt[S => Z, S => Z, S]
    case Put[S, T, Z](t: T) extends PSt[T => Z, S => Z, S]

  def get[S, Z]: Freer[PSt, S => Z, S => Z, S] = Inject(PSt.Get())
  def put[S, T, Z](t: T): Freer[PSt, T => Z, S => Z, S] = Inject(PSt.Put(t))

  /** an indexed natural transformation: every operation becomes the
   * shift body it means — `PState.getAt`/`setAt`, chosen by the
   * handler rather than written into the program */
  val toShift: [S, R, X] => PSt[S, R, X] => (X => S) => R =
    [S, R, X] => (op: PSt[S, R, X]) => (k: X => S) => op match
      // type-test patterns, so the case's own `s0`/`z` are in scope: the
      // body is written at ITS type and is an `R` by the GADT bound —
      // `R` alone, a bare type parameter, cannot be the expected type of
      // a lambda
      // `k(s)(s)` cannot be written: Generate.scala's seed-side `apply`
      // is in lexical scope for package okay and takes the second call
      // (`Cont`'s "NO `apply` extension" comment) — the continuation's
      // answer is named at its own type first
      case _: PSt.Get[s0, z] => val f: s0 => z = s => { val g: s0 => z = k(s); g(s) }; f
      case p: PSt.Put[s0, t, z] => val f: s0 => z = s => { val g: t => z = k(s); g(p.t) }; f

  /**
   * Cont's runner at ANY signature, the handler supplying the leaf's
   * body: `ProbeFreerStep.Cont.run` with `h` where the leaf was the
   * function. Typed by the GADT end to end — `Return` gives `S <: R`,
   * so `k(a): S` IS the `R`. The re-entry is direct style's own frame,
   * as in the library's runner; a probe, not a production loop.
   */
  def run[G[_, +_, +_], S, R, A](p: Freer[G, S, R, A])
                                 (h: [s, r, x] => G[s, r, x] => (x => s) => r)
                                 (k: A => S): R =
    (p.resume: @unchecked) match
      case Return(a) => k(a)
      case Inject(g) => h(g)(k)
      case Bind(Inject(g), f) => h(g)(x => run(f(x))(h)(k))

  test("typestate as an indexed effect: Int -> String -> List[String], the type moving on the tree") {
    val p: Freer[PSt, List[String] => (List[String], Int), Int => (List[String], Int), Int] =
      for
        n <- get[Int, (List[String], Int)]
        _ <- put[Int, String, (List[String], Int)]((n * 2).toString)
        s <- get[String, (List[String], Int)]
        _ <- put[String, List[String], (List[String], Int)](List(s, s))
      yield n + s.length
    val (state, value) = run(p)(toShift)(a => s => (s, a))(21)
    assertEquals(state, List("42", "42"))
    assertEquals(value, 23)
  }

  test("the same program through the abstract ParaMonad, at Freer.Para[PSt]") {
    type Z = (String, String)
    val p = chain[Freer.Para[PSt]](get[Int, Z])(n => put[Int, String, Z](n.toString))(_ => get[String, Z])
    assertEquals(run(p)(toShift)(a => s => (s, a))(7), ("7", "7"))
  }

  test("an operation whose index does not meet the continuation's is refused") {
    val errors = compileErrors("""
      val bad = get[Int, Unit].flatMap(n => put[String, Int, Unit](n))
    """)
    assert(errors.nonEmpty, "a Put from String after a Get of Int must not type")
  }

  // ------------------ reading 1 INSIDE THE EFFECT SYSTEM: a row, a handler

  /**
   * A UNARY effect on the DIAGONAL of an indexed row. `Lift[F]` puts a
   * unary operation at ANY index, and that is exactly what a handler's
   * loop over a mixed row cannot use: matching `Bind(Inject(op), k)`
   * makes the middle index existential, and an answer built from
   * `k`'s program sits at that index where the loop owes one at `R`.
   * `Op` says the operation moves nothing — `T = R`, read off the GADT
   * — so the continuation's program IS an `R`-indexed one by `+R`. One
   * wrapper per unary operation; the allocation-free twin is `Free.
   * Bind`'s trade — an extractor claiming "a unary op is diagonal" by
   * ONE cast, as it claims `Unit` today.
   */
  enum At[F[+_], S, +R, +X]:
    case Op[F[+_], R, X](e: F[X]) extends At[F, R, R, X]

  /** the row: the indexed effect beside an ordinary `State % Int` */
  type Row = [S, R, X] =>> PSt[S, R, X] | At[State[Int, *], S, R, X]

  def rget[S, Z]: Freer[Row, S => Z, S => Z, S] = Inject(PSt.Get())
  def rput[S, T, Z](t: T): Freer[Row, T => Z, S => Z, S] = Inject(PSt.Put(t))
  def tick[R]: Freer[Row, R, R, Int] = Inject(At.Op[State[Int, *], R, Int](State.Modify[Int, Int](_ + 1)))

  /**
   * `State.handle` over the indexed row: its own operations answered
   * from the threaded `Int`, every other operation FORWARDED WITH THE
   * INDEX IT CAME WITH — the shape every handler in the library has,
   * at indexes that are no longer `Unit`. Typed by the GADT: a `State`
   * arm continues at `T <: R`, a forwarded `PSt` op keeps `(T, R)` and
   * the continuation closes `(S, T)`.
   */
  def counted[S, R, A](s: Int)(p: Freer[Row, S, R, A]): Freer[PSt, S, R, (Int, A)] =
    def step[T, X](o: Row[T, R, X], k: X => Freer[Row, S, T, A]): Freer[PSt, S, R, (Int, A)] = o match
      case At.Op(e) => e match
        case State.Get() => counted(s)(k(s))
        case State.Set(n) => counted(n)(k(n))
        case State.Modify(f) => val n = f(s); counted(n)(k(n))
        case State.Update(f) => val (b, n) = f(s); counted(n)(k(b))
      // the library's `split` does this by `TypeableK`; a probe tests the class
      case o: PSt[T, R, X] @unchecked => Inject(o).flatMap(x => counted(s)(k(x)))
    (p.resume: @unchecked) match
      case Return(a) => Return((s, a))
      case Inject(o) => step(o, x => Return(x))
      case Bind(Inject(o), k) => step(o, k)

  test("an indexed effect in a ROW beside State: State's handler forwards the index it does not own") {
    type Z = (List[String], (Int, Int))
    val p: Freer[Row, List[String] => Z, Int => Z, Int] =
      for
        _ <- tick[Int => Z]
        n <- rget[Int, Z]
        _ <- tick[Int => Z]
        _ <- rput[Int, List[String], Z](List.fill(n)("x"))
        c <- tick[List[String] => Z]
      yield c
    val (state, (counter, value)) = run(counted(0)(p))(toShift)(a => s => (s, a))(2)
    assertEquals(state, List("x", "x"))
    assertEquals(counter, 3)
    assertEquals(value, 3)
  }

  // ------- reading 2 on an INVARIANT base: ProbeMcBride, the loop that types

  test("on an invariant base the consumed-state loop types, @tailrec, with no continuation object") {
    assertEquals(ProbeMcBride.answers, (List("21", "21"), 23))
    assert(ProbeMcBride.refusedPut.contains("Required"), ProbeMcBride.refusedPut)
    assert(ProbeMcBride.refusedRun.contains("Required"), ProbeMcBride.refusedRun)
  }

  // ------------------- reading 2: the index is a CONSUMED state (refused)

  /** McBride's shape: `R` the state before, `S` the state after */
  enum St[S, +R, +X]:
    case Get[S]() extends St[S, S, S]
    case Put[S, T](t: T) extends St[T, S, Unit]

  test("a handler that CONSUMES the index cannot type its Return arm on a base covariant in R") {
    val errors = compileErrors("""
      def runSt[S, R, A](p: Freer[St, S, R, A])(r: R): (S, A) =
        (p.resume: @unchecked) match
          case Return(a) => (r, a)
          case Bind(Inject(St.Get()), k) => runSt(k(r))(r)
          case Bind(Inject(St.Put(t)), k) => runSt(k(()))(t)
    """)
    // EVERY arm that READS the state fails, and only those: `Return`
    // holds an R and owes an S with `S <: R`; `Get` hands `r: R` to a
    // continuation whose argument the match bound as a SUPERtype of the
    // case's own state, not R itself (the signature is covariant in R
    // and X because the base's bound says so, so the GADT gives bounds
    // where an invariant enum gave equalities). `Put`, which PRODUCES
    // the next state, types. Predicted before compiling: the Return arm
    // alone; the compiler added Get.
    assert(errors.contains("Required: S"), errors)
    assert(errors.contains("case Bind(Inject(St.Get()), k)"), errors)
    assert(!errors.contains("St.Put"), s"the producing arm failed too:\n$errors")
    assertEquals(errors.linesIterator.count(_.startsWith("error")), 2, errors)
  }
