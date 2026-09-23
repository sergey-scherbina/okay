package okay


/**
 * THE PARALLEL APPLICATIVE (stage 1 of specs/applicative-static.md).
 *
 * An applicative program is a pure lambda term over effectful
 * ARGUMENTS: `pure(f) <*> fa <*> fb`. What it cannot do is bind one
 * leaf's answer into another leaf's body — and that is why the leaves
 * may run at once. `Par` is that reading of `A ! Async`: `app` joins
 * two leaves with `Async.par`, which forks both, fails the pair on
 * either failure and cancels the healthy sibling. That symmetry is
 * inherited and was not free: this carrier's own test found `par`
 * watching only its left side, and the fix is par-fail-fast
 * (BUGS.md, par-right-failure-waits).
 *
 * So generic applicative code becomes parallel by CHOOSING AN
 * INSTANCE, with nothing in the program changed: `traverse`,
 * `sequence`, `replicateA`, `*>`, `<*` and anything else written
 * against `Applicative[F]` runs its leaves at once at this carrier
 * and in order at the program's own.
 *
 * WHAT THIS IS NOT: `parAll` and `parTraverse`
 * (src/main/scala-jvm-native/Parallel.scala) already run a FLAT
 * sequence of programs with a fiber each, joined in order, and they
 * stay. The three differences are worth knowing before choosing:
 * they are JVM/Native only (a blocking join needs CanBlock — this
 * carrier parks nothing and runs on JS), they do not cancel the
 * siblings of a leaf that failed (they spawn everything, then join
 * in order), and they take a SEQUENCE. `Par` takes a spine: a
 * lambda term over leaves of DIFFERENT types, or generic code that
 * never heard of Async. For a flat sequence of same-typed programs
 * on the JVM, `parAll` is one fiber per leaf and this is two — reach
 * for it there.
 *
 * `Par` IS NOT A MONAD, and the omission is the design (Marlow et
 * al., "There is no fork: an abstraction for efficient, concurrent
 * and concise data access", ICFP 2014). A `flatMap` would sequence
 * the spine again while the type still said the leaves were
 * independent — the parallelism would vanish exactly where it was
 * written most naturally. A program that needs one answer to build
 * the next is `A ! Async` and says so.
 *
 * WHY THE CARRIER IS OPAQUE INSIDE THIS OBJECT rather than at top
 * level: a top-level `opaque type` is transparent to its whole
 * PACKAGE (Cont.scala says the same, and paid for it), so declared
 * there `Par[A]` would still be plainly `A ! Async` everywhere in
 * `okay` — and `Free`'s own `Monad` would answer `Applicative[Par]`
 * inside the package, sequentially, from the same call. Inside an
 * object the scope is the object.
 */
object Par:

  /** the representation: a program, read as ONE LEAF of a spine */
  opaque type Rep[A] = A ! Async

  /** this program is a leaf — the door in */
  inline def apply[A](p: A ! Async): Rep[A] = p

  extension [A](p: Rep[A])
    /** the spine as an ordinary program, its leaves already joined by
     * `Async.par` wherever the spine said `app` — the door out */
    inline def seq: A ! Async = p

  /**
   * Two leaves at once, joined by a plain function — the workhorse
   * for a spine whose leaves have DIFFERENT types. (`Par(p).map(f)`
   * is the instance's own map since comonad-id-map-capture; before
   * it, a package-level `Comonad[Id]` won that race and this method
   * was the only spelling that could not lose it.)
   */
  def map2[A, B, C](a: Rep[A], b: Rep[B])(f: (A, B) => C)(using Scheduler): Rep[C] =
    Async.par(a, b).map(f(_, _))

  /** every element at once, results in the argument's order — the
   * generic `traverse` at this carrier, named so a call site says
   * which one it meant */
  def traverse[A, B](xs: Seq[A])(f: A => B ! Async)(using Scheduler): Seq[B] ! Async =
    okay.traverse(xs)(a => apply(f(a))).seq

  /** every program at once, results in the argument's order */
  def sequence[A](xs: Seq[A ! Async])(using Scheduler): Seq[A] ! Async =
    traverse(xs)(identity)

  /**
   * `app` FORKS; `fmap` does not.
   *
   * `Applicative.fmap`'s default is `pure(f).app(a)`, which through
   * this instance would fork a fiber to hold a pure function and
   * another to run the only leaf there is. `map` on the program is
   * the same answer with neither. It matters because `traverse`
   * calls `fmap` once per element.
   */
  given parApplicative(using Scheduler): Applicative[Rep] with
    def pure[A](a: A): Rep[A] = !.pure(a)
    override def fmap[A, B](a: Rep[A], f: A => B): Rep[B] = a.map(f)
    extension [A, B](f: Rep[A => B])
      def app(a: Rep[A]): Rep[B] = Async.par(f, a).map((g, x) => g(x))

/**
 * The carrier's name outside its companion, where it is abstract —
 * `Par[A]` is a program that may be run beside another, and nothing
 * but `Par.apply` makes one.
 */
type Par[A] = Par.Rep[A]
