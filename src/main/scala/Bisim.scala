package okay

import scala.annotation.tailrec
import Free.*

/**
 * ARE THESE TWO PROGRAMS THE SAME PROGRAM? An executable answer, for
 * the rewrites this library makes of its own programs (specs/handler-
 * equivalence-oracle.md).
 *
 * A program here is a tree that stops at every operation, so two
 * programs can be compared the way normal-form bisimilarity compares
 * terms (Biernacki, Lenglet & Polesiuk, "A complete normal-form
 * bisimilarity for algebraic effects and handlers", FSCD 2020): walk
 * both to their next operation, demand the SAME operation, then feed
 * both continuations the same answer and go on. Two programs that
 * agree on every path are indistinguishable by ANY handler, because a
 * handler sees nothing but the operations and chooses nothing but the
 * answers. It is equivalence of interaction trees in the sense of Xia
 * et al. (POPL 2020), without the silent steps: `Free.resume` has
 * already removed them.
 *
 * WHAT MAKES IT EXECUTABLE is that the answers are a SAMPLE, not a
 * symbol. `Answers[F]` names a finite list of answers per operation,
 * and the walk tries each, to a depth bound. Hence the two verdicts
 * are not symmetric:
 *   - `Differ` is a PROOF: a concrete path of operations and answers
 *     on which the two programs part, printed.
 *   - `Same` is EVIDENCE: every sampled path agreed, and it says how
 *     many ended, and how many the depth bound cut still running. A
 *     caller asserts on those counts, since a `Same` over zero paths
 *     says nothing.
 *
 * WHAT IT DOES NOT CHECK is equivalence under ONE handler's
 * equations. `get; get` and `get` differ here (a handler could answer
 * the two gets differently) and agree under `State.run`. For a
 * rewrite that is sound only by an effect's laws, compare the
 * programs AFTER that effect's handler, on the residual row.
 *
 * No cast: the two programs' operations are compared with `==` and
 * then each is answered from ITS OWN call to `Answers`, zipped. That
 * is sound because `Answers` is a function of the operation, so equal
 * operations get equal lists. When the lists differ anyway, that is
 * reported as a `Differ` naming the answers rather than trusted.
 */
object Bisim:

  /**
   * The answers the walk feeds an operation: a finite sample, and a
   * FUNCTION of the operation. An empty list makes the operation a
   * leaf (`Stop.Now`, a raise): nothing continues after it.
   */
  trait Answers[F[+_]]:
    def apply[X](op: F[X]): List[X]

    /** a row of two: each operation is answered by the member whose
     * test takes it (`split`, the same test a runner uses) */
    infix def +[G[+_]](g: Answers[G])(using TypeableK[F]): Answers[F + G] =
      val f = this
      new Answers[F + G]:
        def apply[X](op: F[X] | G[X]): List[X] = split[F, G](op)(f(_))(g(_))

  object Answers:
    /** every `get` answered by each sample in turn; a `set` by what it set */
    def state[S](samples: S*): Answers[State % S] = new Answers[State % S]:
      def apply[X](op: State[S, X]): List[X] = op match
        case State.Get() => samples.toList
        case State.Set(s) => List(s)

    /** every `ask` answered by each sample in turn */
    def reader[R](samples: R*): Answers[Reader % R] = new Answers[Reader % R]:
      def apply[X](op: Reader[R, X]): List[X] = op match
        case Reader.Ask() => samples.toList

    /** a tell has one answer, and the told value is compared as part
     * of the operation */
    def writer[W]: Answers[Writer % W] = new Answers[Writer % W]:
      def apply[X](op: Writer[W, X]): List[X] = op match
        case Writer.Say(_) => List(())

    /** `Stop.Now` is a leaf: both programs must stop HERE */
    val stop: Answers[Stop] = new Answers[Stop]:
      def apply[X](op: Stop[X]): List[X] = op match
        case Stop.Now => Nil

  /** what the walk found */
  enum Verdict:
    /** every sampled path agreed: `paths` of them ended (equal values,
     * or the same leaf operation), `cut` were still running at the
     * depth bound or past the path budget */
    case Same(paths: Int, cut: Int)
    /** the first path on which the programs part: each step is the
     * operation both performed and the answer fed to it */
    case Differ(path: List[String], left: String, right: String)

    def same: Boolean = this match
      case Same(_, _) => true
      case Differ(_, _, _) => false

    override def toString: String = this match
      case Same(p, c) => s"Same($p paths ended, $c cut)"
      case Differ(path, l, r) =>
        val at = if path.isEmpty then "at the start" else path.mkString("after ", " ; ", "")
        s"Differ $at: left $l, right $r"

  /**
   * Walk `p` and `q` in lockstep over every sampled answer, to `depth`
   * operations per path and at most `budget` paths in all. Depth-first
   * with an explicit stack, so neither a long path nor a wide tree
   * grows the JVM stack.
   */
  def check[F[+_], A](p: A ! F, q: A ! F, depth: Int = 32, budget: Int = 100_000)
                     (using ans: Answers[F]): Verdict =
    @tailrec def go(todo: List[Frame[F, A]], paths: Int, cut: Int): Verdict = todo match
      case Nil => Verdict.Same(paths, cut)
      case _ if paths + cut >= budget => Verdict.Same(paths, cut + todo.size)
      case Frame(path, l, r, d) :: rest => (view(l), view(r)) match
        case (Left(a), Left(b)) =>
          if a == b then go(rest, paths + 1, cut)
          else Verdict.Differ(path.reverse, s"returned $a", s"returned $b")
        case (Left(a), Right(s)) => Verdict.Differ(path.reverse, s"returned $a", s"performed ${s.show}")
        case (Right(s), Left(b)) => Verdict.Differ(path.reverse, s"performed ${s.show}", s"returned $b")
        case (Right(sl), Right(sr)) =>
          if !sl.sameOp(sr) then Verdict.Differ(path.reverse, s"performed ${sl.show}", s"performed ${sr.show}")
          else
            val nl = sl.next(ans)
            val nr = sr.next(ans)
            if nl.map(_._1) != nr.map(_._1) then
              Verdict.Differ(path.reverse, s"${sl.show} answered ${nl.map(_._1)}",
                s"${sr.show} answered ${nr.map(_._1)} (Answers is not a function of the operation)")
            else if nl.isEmpty then go(rest, paths + 1, cut)
            else if d >= depth then go(rest, paths, cut + 1)
            else
              val more = nl.lazyZip(nr).map { case ((x, kl), (_, kr)) =>
                Frame(s"${sl.show} -> $x" :: path, kl, kr, d + 1) }
              go(more ++ rest, paths, cut)
    go(List(Frame(Nil, p, q, 0)), 0, 0)

  /** one pending comparison: the path so far (newest first), the two
   * programs from there, and how many operations the path has */
  private final case class Frame[F[+_], A](path: List[String], l: A ! F, r: A ! F, d: Int)

  /** an operation and its continuation, the answer type kept */
  private final class Step[F[+_], A, X](op: F[X], k: X => A ! F):
    def show: String = String.valueOf(op)
    def sameOp(that: Step[F, A, ?]): Boolean = that.is(op)
    private def is(other: Any): Boolean = op == other
    /** each sampled answer, rendered, with where it leads */
    def next(ans: Answers[F]): List[(String, A ! F)] =
      ans(op).map(x => (String.valueOf(x), k(x)))

  /** the head form `resume` leaves: a value, or an operation to answer */
  private def view[F[+_], A](p: A ! F): Either[A, Step[F, A, ?]] = (p.resume: @unchecked) match
    case Return(a) => Left(a)
    case Inject(e) => Right(Step[F, A, A](e, Return(_)))
    case Bind(Inject(e), k) => Right(Step(e, k))
