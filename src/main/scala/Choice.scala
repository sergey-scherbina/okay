package okay


/**
 * The nondeterminism effect: choose one of several values, and let
 * the handler explore every branch. The handler is MULTI-SHOT — it
 * invokes the captured continuation once per alternative, which is
 * delimited continuations doing what neither a relay (exactly-once
 * by parametricity) nor an ordinary exception-style handler can.
 * Each nesting level of choose costs stack at run time.
 */
case class Choose[+A](as: Seq[A]) extends Final derives Effect:
  /** no alternatives: the branch is pruned and never resumes */
  override def isFinal: Boolean = as.isEmpty

/**
 * A BRANCH POINT SOMETHING IS SHARED THROUGH (logic-cut-releases, specs/backtracking.md): a `Choose` that passed
 * out of a `Resource` scope holding acquisitions carries one — the scope's `Fork` — so a handler that will start
 * no more of its alternatives can say so (`abandon`), and what the branches share is released once nothing can
 * use it. A split's rest carries the ones it still holds (a `Group`).
 */
trait Shared:
  /** no more alternatives will be started: what only they would have used can go */
  def abandon(): Unit

object Shared:
  /** several branch points, abandoned together */
  final class Group(members: List[Shared]) extends Shared:
    def abandon(): Unit = members.foreach(_.abandon())

/**
 * The alternatives of a `Choose` with the branch point they pass through (`Choose(Forked(as, s))` is still a
 * `Choose` of a `Seq`, to every handler). Its end is told to the branch point when it counts (`Forked.Counting`):
 * how many a lazy list held is known only there.
 */
final class Forked[+A](val under: Seq[A], val shared: Shared) extends scala.collection.immutable.AbstractSeq[A]:
  def apply(i: Int): A = under(i)
  def length: Int = under.length
  override def knownSize: Int = under.knownSize
  override def isEmpty: Boolean = under.isEmpty
  def iterator: Iterator[A] = shared match
    case c: Forked.Counting =>
      val it = under.iterator
      new Iterator[A]:
        private var i = 0
        def hasNext: Boolean = { val h = it.hasNext; if !h then c.exhausted(i); h }
        def next(): A = { val a = it.next(); i += 1; a }
    case _ => under.iterator

object Forked:
  /** a branch point that counts the alternatives a handler takes from it */
  trait Counting extends Shared:
    /** there are `n`, all taken */
    def exhausted(n: Int): Unit

  /** the branch points `as` carries, outermost scope first */
  def sharedOf(as: Seq[?]): List[Shared] =
    @scala.annotation.tailrec def go(as: Seq[?], acc: List[Shared]): List[Shared] = as match
      case f: Forked[?] => go(f.under, f.shared :: acc)
      case _ => acc.reverse
    go(as, Nil)

/** The class IS the whole identity: Choose has no parameter but its
 * (erased) answer type, so splitting a row on it is a TOTAL test —
 * said once here, rather than as a "cannot be checked at runtime"
 * warning at every use site of a test that is in fact complete. */


/** one of the given alternatives */
inline def choose[A](as: A*): A ! Choose = effect(Choose(as))

/**
 * Nondeterminism is the canonical MonadPlus: no alternatives is
 * failure (the handler prunes the branch), append chooses between two
 * whole computations. Note the overlap: Monad[Free[F, *]] also covers
 * Choose programs — summon MonadPlus explicitly where empty/append
 * are needed.
 */
given MonadPlus[[A] =>> A ! Choose] with
  override def pure[A](a: A): A ! Choose = okay.pure(a)
  override def empty[A]: A ! Choose = effect(Choose(Seq.empty))
  extension [A](x: A ! Choose)
    override def flatMap[B](f: A => B ! Choose): B ! Choose = x.flatMap(f)
    override def append(y: A ! Choose): A ! Choose =
      effect[Choose, A ! Choose](Choose(Seq(x, y))).flatMap(identity)

/**
 * A row CONTAINING Choose is a MonadPlus too — which is what lets
 * `guard` prune inside an effectful search (the model is asked, the
 * answer is judged, the branch dies or lives).
 */
given [F[+_]]: MonadPlus[[A] =>> A ! Choose + F] with
  override def pure[A](a: A): A ! Choose + F = okay.pure(a)
  override def empty[A]: A ! Choose + F = effect(Choose(Seq.empty))
  extension [A](x: A ! Choose + F)
    override def flatMap[B](f: A => B ! Choose + F): B ! Choose + F = x.flatMap(f)
    override def append(y: A ! Choose + F): A ! Choose + F =
      effect[Choose + F, A ! Choose + F](Choose(Seq(x, y))).flatMap(identity)

object Choose:
  /** a handler that will start no more of `c`'s alternatives says so: what they share is released when nothing
   * else can use it (logic-cut-releases). A handler that stops early without saying so keeps it open */
  def abandon(c: Choose[?]): Unit = Forked.sharedOf(c.as).foreach(_.abandon())

  /** the handler as a value: `p.handle(Choose.all)` answers every branch's result, in order */
  def all: Handler[Choose, Seq] = new Handler[Choose, Seq]:
    def run[A, F[+_]](p: A ! Choose + F)(using A <:< Any, Distinct[Choose + F], Handler.Nothing[F]): Seq[A] ! F =
      runChoice(p)

/** all the results of all the branches, forwarding the effects F */
def runChoice[A, F[+_]](a: A ! Choose + F): Seq[A] ! F =
  val E = Effects[Free]
  E.handle[Choose, F](a)(x => pure(Seq(x))):
    [X] => c => E.control.shift: k =>
      val all = okay.!.foldM(c.as)(Seq.empty[A])((s, x) => k(x).map(s ++ _))
      // a branch point something is shared through: a throw that leaves the search abandons it (logic-cut-releases)
      c.as match
        case _: Forked[?] => HandleFrames.unwinding[Seq[A], F](() => Choose.abandon(c))(all)
        case _ => all

/**
 * A COLLECTION IS ALREADY A SIGNATURE, and this is the sharpest
 * statement of what a freer monad is: ANY type constructor can be an
 * effect, so `List[A]` — which already means "several A" — is
 * nondeterminism without a wrapper.
 *
 *     val pairs: (Int, Int) ! List =
 *       for
 *         x <- List(1, 2).perform
 *         y <- List(10, 20).perform
 *       yield (x, y)
 *
 *     runSeq[List, (Int, Int), Pure](pairs)
 *     // List((1,10), (1,20), (2,10), (2,20))
 *
 * The handler is `runChoice`'s, unchanged, because there was never
 * anything else in it: `Choose[+A](as: Seq[A])` is a box around a Seq
 * of alternatives, and the box is what this does without. `Choose`
 * keeps its own name and its place in a row — a row wants a signature
 * that means nondeterminism and nothing else, and `List` in a row
 * means whatever the reader guesses — but seeing that the two are the
 * same handler is the point.
 */
def runSeq[S[+X] <: Seq[X], A, F[+_]](p: A ! S + F)(using TypeableK[S]): Seq[A] ! F =
  val E = Effects[Free]
  E.handle[S, F](p)(x => pure(Seq(x))):
    [X] => (s: S[X]) => E.control.shift: k =>
      okay.!.foldM(s)(Seq.empty[A])((prev, x) => k(x).map(prev ++ _))

/** the class IS the identity for a collection too: the element type
 * is erased, so the test is total for exactly the reason `Choose`'s
 * is */
given TypeableK[List] = typeableK(classOf[List[?]])
given TypeableK[Vector] = typeableK(classOf[Vector[?]])
