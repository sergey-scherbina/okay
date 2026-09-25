package okay

import scala.annotation.tailrec

/**
 * THE FREE SELECTIVE (stage 2 of specs/applicative-static.md): a
 * program whose EFFECTS ARE KNOWN BEFORE IT RUNS.
 *
 * `Free` is the free MONAD, and its `Bind` carries a function — so
 * what a program does after its first operation is a host closure
 * nobody can read without running it. That opacity is the price of
 * `flatMap`, and most programs are worth it.
 *
 * Some are not. A program that fetches twenty keys, or declares what
 * a module needs, or explains a rule pack before applying it, has a
 * PURE SPINE: a lambda term whose arguments happen to be effects.
 * `Static` is that program as data — Capriotti and Kaposi's free
 * applicative (2014) with Mokhov, Lukyanov, Marlow and Dimino's
 * `select` on top (2019), so a conditional can be written down
 * without a continuation.
 *
 * What it buys, in one line each:
 *
 *   `leaves`   every operation the program MAY perform, before it runs
 *   `toFree`   the same program as `A ! F`, to run in the ordinary way
 *   `foldMap`  the spine interpreted into any Selective — which is
 *              where batching lives: N leaves, one round trip
 *
 * THE APPROXIMATION IS NAMED. `leaves` reports both sides of every
 * `Select`, because which side runs is decided by a value that does
 * not exist yet. It is an upper bound, exact for a spine with no
 * `Select` — and an upper bound is what a batcher, a capability list
 * and a dry run all want anyway.
 *
 * WHY IT IS NOT A NODE IN `Free`. Three reasons, all recorded. `Ap`
 * folds differently from `Bind` (both sides are programs, so the walk
 * is a tree and not a list), a fifth case in that enum re-decides the
 * inlining of every interpreter that matches on it (the core's hot
 * loops sit at the measured threshold — specs/core-cleanup.md), and
 * the guarantee this type sells is that NO `Bind` is present, which a
 * type that has one cannot make. The bridge is one-way and cheap:
 * `toFree`.
 */
enum Static[F[+_], A]:
  /** a value, no operation */
  case Pure(a: A)

  /** one operation of the signature — a leaf */
  case Op(fa: F[A])

  /** a function under effects, applied to an argument under effects:
   * the idiom bracket's `<*>`, as data */
  case Ap[F[+_], A, B](f: Static[F, A => B], a: Static[F, A]) extends Static[F, B]

  /** the conditional with BOTH sides written down: run the scrutinee,
   * and on a `Left` run the handler too (Mokhov et al. 2019) */
  case Select[F[+_], A, B](e: Static[F, Either[A, B]],
                           f: Static[F, A => B]) extends Static[F, B]

  /**
   * A pure function over the answer — one `Ap` of a `Pure`, which is
   * what the instance's `fmap` is. A METHOD, not an extension: it was
   * made one when a package-level `Comonad[Id]` put a lexical `map`
   * on every type (lexical-extension-beats-companion, di-needs-from-
   * static); that given lives in `Comonad`'s companion since
   * comonad-id-map-capture, and a member is found first regardless.
   */
  def map[B](f: A => B): Static[F, B] = Ap(Pure(f), this)

object Static:

  /** one operation as a program — the door in */
  inline def op[F[+_], A](fa: F[A]): Static[F, A] = Op(fa)

  /**
   * The instance. `fmap` is `Ap(Pure(f), _)` rather than the trait's
   * derivation, which is the same thing — but written here it is one
   * node instead of two lookups, and `traverse` calls it once per
   * element.
   */
  given static[F[+_]]: Selective[[A] =>> Static[F, A]] with
    def pure[A](a: A): Static[F, A] = Pure(a)
    override def fmap[A, B](a: Static[F, A], f: A => B): Static[F, B] = Ap(Pure(f), a)
    extension [A, B](f: Static[F, A => B])
      def app(a: Static[F, A]): Static[F, B] = Ap(f, a)
    extension [A, B](e: Static[F, Either[A, B]])
      def select(f: => Static[F, A => B]): Static[F, B] = Select(e, f)

  /**
   * THE ARGUMENTS OF A SPINE, TYPE-ALIGNED — what makes `foldMap`
   * stack-safe WITHOUT a cast.
   *
   * `traverse`'s `foldLeft` builds `Ap(Ap(Ap(base, l1), l2), l3)`, so
   * the recursion that grows is the one on an `Ap`'s FIRST component.
   * Walking it with a plain stack is easy; coming back up is not,
   * because each level's intermediate type is gone and
   * `g.app(argument)` no longer type-checks. Other libraries reach
   * for an internal cast here.
   *
   * It is not needed. `Args[F, T, C]` says "apply these to something
   * of type T and get a C", and its two cases CARRY the alignment:
   * `Done` only exists at `Args[F, C, C]`, and consing an argument of
   * type `X` in front of an `Args[F, R, C]` gives an
   * `Args[F, X => R, C]`. Matching on them refines the types, so the
   * fold back up is ordinary typed code.
   */
  enum Args[F[+_], G[_], T, C]:
    case Done[F[+_], G[_], C]() extends Args[F, G, C, C]
    case More[F[+_], G[_], X, R, C](arg: Static[F, X], rest: Args[F, G, R, C])
      extends Args[F, G, X => R, C]
    /**
     * A PURE function to apply, and it is what makes the walk
     * iterative on the shape that actually occurs.
     *
     * The first `Args` had two cases and still overflowed, because
     * the depth is not where it looks. `traverse`'s `foldLeft` builds
     * `Ap(Ap(Pure(g), acc), leaf)`: walking down FIRST components
     * reaches `Pure(g)` in two steps and pushes the whole accumulator
     * — the deep thing — as an ARGUMENT, to be folded by an ordinary
     * recursive call. The stack came back at the same depth by
     * another road, and a 50 000-leaf test said so.
     *
     * So `Ap(Pure(g), a)` is not an application to walk past: it is
     * "fold `a`, then map it by `g`". Carrying `g` here lets the walk
     * CONTINUE INTO `a` instead of parking it. Sound because the
     * first component is `Pure`: it performs nothing, so running `a`
     * before it changes no order of effects.
     */
    case Mapped[F[+_], G[_], X, R, C](f: X => R, rest: Args[F, G, R, C])
      extends Args[F, G, X, C]
    /**
     * The function side is folded; fold the ARGUMENT next, then apply
     * (stack-safety, 2026-09-25). Until then each argument was folded
     * by an ordinary call, one host frame per level of nesting inside
     * an argument, and a Select's two sides the same way.
     */
    case AppTo[F[+_], G[_], X, R, C](gf: G[X => R], rest: Args[F, G, R, C])
      extends Args[F, G, X, C]
    /** a Select's condition is folded; fold its function side next */
    case SelectE[F[+_], G[_], X, Y, C](f: Static[F, X => Y], rest: Args[F, G, Y, C])
      extends Args[F, G, Either[X, Y], C]
    /** both sides of a Select are folded: select */
    case SelectF[F[+_], G[_], X, Y, C](ge: G[Either[X, Y]], rest: Args[F, G, Y, C])
      extends Args[F, G, X => Y, C]

  /** the walk's two directions, each with the rest of the work: DOWN a
   * program to fold, or UP with a folded value to hand to `args` */
  private enum Step[F[+_], G[_], C]:
    case Down[F[+_], G[_], T, C](s: Static[F, T], args: Args[F, G, T, C]) extends Step[F, G, C]
    case Up[F[+_], G[_], T, C](g: G[T], args: Args[F, G, T, C]) extends Step[F, G, C]

  /**
   * Down the left spine, collecting arguments; then back up,
   * applying them. Both halves are tail-recursive loops, so a spine
   * of any depth costs no host stack.
   *
   * NOTHING RECURSES (stack-safety, 2026-09-25): the arguments and a
   * `Select`'s two sides were folded by ordinary calls until then, and
   * a program nested inside an argument or a condition overflowed a
   * small stack at 3 000 levels. Now every pending piece of work is a
   * frame of `Args`, and one loop, `fold`, walks down and up. One
   * consequence: a `Select`'s function side is folded BEFORE `select`
   * is called, not by-name inside it. The value is the same, because
   * folding only builds a `G`. The difference is that a `G` whose
   * `select` never reads that side still receives it folded.
   */
  @tailrec private def fold[F[+_], G[_], C](step: Step[F, G, C], nt: F ==> G)(using G: Selective[G]): G[C] =
    step match
      case d: Step.Down[F, G, t, C] => d.s match
        // the shape every fold builds: a pure function applied to a
        // deep accumulator — walk INTO the accumulator (see Args.Mapped)
        case Ap(Pure(g), a) => fold(Step.Down(a, Args.Mapped(g, d.args)), nt)
        case Ap(f, a) => fold(Step.Down(f, Args.More(a, d.args)), nt)
        case Pure(a) => fold(Step.Up(G.pure(a), d.args), nt)
        case Op(fa) => fold(Step.Up(nt(fa), d.args), nt)
        // the condition first, then the function side (SelectE, SelectF)
        case Select(e, f) => fold(Step.Down(e, Args.SelectE(f, d.args)), nt)
      case u: Step.Up[F, G, t, C] => u.args match
        // `@unchecked` on the TYPE ARGUMENTS, and it is the same claim
        // `Free.resume`'s callers make: the class test is total over
        // this enum's cases, and the type arguments are the ones each
        // case was built with — `Done` exists only at
        // `Args[F, G, C, C]`, `More` only at `Args[F, G, X => R, C]`,
        // and so on. What the compiler cannot check at run time, the
        // constructors guaranteed at compile time.
        //
        // Each value is NAMED at its refined type (`val gf: G[x => r]`)
        // because the match refines `t`, but `u.g` is still written
        // `G[t]`, and `app`/`select` cannot find their shapes through
        // the alias.
        case _: (Args.Done[F, G, C] @unchecked) => u.g
        case m: (Args.More[F, G, x, r, C] @unchecked) =>
          val gf: G[x => r] = u.g
          fold(Step.Down(m.arg, Args.AppTo(gf, m.rest)), nt)
        case m: (Args.AppTo[F, G, x, ?, C] @unchecked) =>
          val gx: G[x] = u.g
          fold(Step.Up(m.gf.app(gx), m.rest), nt)
        case m: (Args.Mapped[F, G, x, ?, C] @unchecked) =>
          val gx: G[x] = u.g
          fold(Step.Up(G.fmap(gx, m.f), m.rest), nt)
        case m: (Args.SelectE[F, G, x, y, C] @unchecked) =>
          val ge: G[Either[x, y]] = u.g
          fold(Step.Down(m.f, Args.SelectF(ge, m.rest)), nt)
        case m: (Args.SelectF[F, G, x, y, C] @unchecked) =>
          val gf: G[x => y] = u.g
          fold(Step.Up(m.ge.select(gf), m.rest), nt)

  extension [F[+_], A](s: Static[F, A])

    /**
     * Every operation this program MAY perform, in program order.
     *
     * `F[Any]`, not `F[?]`: a wildcard application of a
     * higher-kinded parameter is unreducible here (E043, the same
     * wall specs/schema-fold.md hit), and it is not needed — every
     * signature in this library is COVARIANT, so `F[X] <: F[Any]`
     * for every X and the widening is an upcast the compiler makes
     * on its own. No cast, and the caller still matches on the
     * operation's own cases.
     *
     * AN EXPLICIT STACK, not structural recursion, and the reason is
     * this type's own shape: `traverse`'s `foldLeft` builds the spine
     * LEFT-NESTED, so N elements are N `Ap` nodes deep. MEASURED
     * 2026-09-17 on a traverse-built spine, default JVM stack: the
     * same walk written recursively returns at 10 000 leaves and
     * overflows at 50 000; this loop returns at 50 000.
     */
    def leaves: Vector[F[Any]] =
      val out = Vector.newBuilder[F[Any]]
      var todo: List[Static[F, ?]] = s :: Nil
      while todo.nonEmpty do
        val head = todo.head
        todo = todo.tail
        head match
          case Pure(_) => ()
          case Op(fa) => out += fa
          case Ap(f, a) => todo = f :: a :: todo
          case Select(e, f) => todo = e :: f :: todo
      out.result()

    /**
     * The same program, monadic — to RUN it with everything the
     * library already has: handlers, rows, `runWith`, `foldCont`.
     *
     * `Ap` becomes a right-nested `Bind`, which is the shape
     * `Free.resume` is fastest on, and `Select` runs its scrutinee
     * and then AT MOST ONE side — the difference from `leaves`, made
     * good at run time.
     *
     * DEFERRED, because the spine is left-nested: `Free.defer` puts
     * the recursive step behind a `Delay` the interpreter's own loop
     * forces, so converting a spine of any depth costs no host stack.
     * Each node earns its `Delay` (a bare `delay(t)` where there is
     * nothing to do afterwards — defer-with-pure-left-nests).
     */
    def toFree: A ! F = s match
      case Pure(a) => Free.Return(a)
      case Op(fa) => Free.Inject(fa)
      case Ap(f, a) => Free.defer(() => f.toFree)(g => a.now.map(g))
      case Select(e, f) =>
        Free.defer(() => e.toFree):
          case Left(x) => f.now.map(_(x))
          case Right(b) => Free.Return(b)

    /**
     * The right-hand side of an `Ap`, converted — and the `Delay`
     * EARNED rather than spent.
     *
     * The left spine is what grows: `traverse`'s `foldLeft` nests on
     * the first component, so only THAT recursion needs deferring.
     * The right component is a leaf in every spine a fold builds, and
     * a leaf needs no trampoline — `Free.delay(() => Inject(fa))` is
     * a node and a closure to arrive at a node. MEASURED 2026-09-17
     * at 1000 leaves, prebuilt against prebuilt: deferring both sides
     * cost 84.3 µs and 899 105 B/op; earning the node costs 80.7 µs
     * and 843 049 B/op — exactly 56 bytes per leaf less, which is the
     * `Delay` and its thunk, and the byte figure is the load-proof
     * one. It does NOT close the gap to the monadic program (46.8 µs,
     * 475 088 B/op): converting a tree into a tree is the cost, and
     * the spec's Results say so rather than hiding it. Only a
     * right-nested `Ap` pays the fallback, and only for its own
     * depth (TestStatic has the 10 000-deep right-nested spine).
     */
    private def now: A ! F = s match
      case Pure(a) => Free.Return(a)
      case Op(fa) => Free.Inject(fa)
      case _ => Free.delay(() => s.toFree)

    /**
     * The spine interpreted into another Selective — the natural
     * transformation, and the door batching goes through: a carrier
     * whose `app` ACCUMULATES its leaves' requests and whose run
     * answers them in one call turns N operations into one round
     * trip, with the program unchanged.
     *
     * It asks for a `Selective[G]`, not an `Applicative[G]`, and the
     * reason is `Select`: an applicative carrier has no way to run
     * one side and not the other. A carrier that wants to run both
     * (a batcher must: it fetches what the program MIGHT ask for)
     * says so by implementing `select` as `selectA` — lawful, and
     * exactly the over-approximation `leaves` reports.
     *
     * IT IS STACK-SAFE SINCE static-foldmap-stack-safe (2026-09-18),
     * and the record of what it cost is worth keeping. It folded
     * 5 000 leaves and overflowed at 10 000, and this comment used to
     * say it "cannot be a loop without reassembling existentials".
     * That was half right: the reassembly is the difficulty, and it
     * does NOT need the cast other libraries use for it — a
     * type-aligned `Args` (below) carries the alignment in its own
     * constructors, so coming back up is ordinary typed code. 50 000
     * leaves now fold, which is where the RECURSIVE walk of `leaves`
     * used to die.
     */
    def foldMap[G[_]](nt: F ==> G)(using G: Selective[G]): G[A] =
      Static.fold(Static.Step.Down(s, Static.Args.Done[F, G, A]()), nt)
