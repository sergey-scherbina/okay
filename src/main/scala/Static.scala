package okay

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
      def select(f: Static[F, A => B]): Static[F, B] = Select(e, f)

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
      case Pure(a) => Free.Pure(a)
      case Op(fa) => Free.Inject(fa)
      case Ap(f, a) => Free.defer(() => f.toFree)(g => a.now.map(g))
      case Select(e, f) =>
        Free.defer(() => e.toFree):
          case Left(x) => f.now.map(_(x))
          case Right(b) => Free.Pure(b)

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
      case Pure(a) => Free.Pure(a)
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
     * IT IS THE ONE DOOR HERE THAT IS NOT STACK-SAFE, and the number
     * is measured rather than feared: on a traverse-built spine at
     * the default JVM stack it folded 5 000 leaves and overflowed at
     * 10 000 (2026-09-17). Unlike `leaves` and `toFree` it cannot be
     * a loop without reassembling existentials — the `G` values must
     * be combined on the way back up, and the type of each level's
     * intermediate is gone — so the honest answer today is the bound
     * and a batcher that chunks. `leaves` and `toFree`, the two that
     * must scale, do (BACKLOG: static-foldmap-stack-safe).
     */
    def foldMap[G[_]](nt: F ==> G)(using G: Selective[G]): G[A] = s match
      case Pure(a) => G.pure(a)
      case Op(fa) => nt(fa)
      case Ap(f, a) => f.foldMap(nt).app(a.foldMap(nt))
      case Select(e, f) => e.foldMap(nt).select(f.foldMap(nt))
