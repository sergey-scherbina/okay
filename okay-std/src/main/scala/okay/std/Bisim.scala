package okay.std

import okay.{!, +, %}
type Row = okay.one.row.Pure
import okay.one.row.In
import okay.one.machine.Machine
import okay.one.machine.Machine.Head

import scala.annotation.tailrec

/**
 * ARE THESE TWO PROGRAMS THE SAME PROGRAM? An executable answer, for
 * the rewrites this library makes of its own programs (specs/handler-
 * equivalence-oracle.md).
 *
 * A program here is STEPPED (okay-core's `Machine.Head`): it stops at
 * every operation and hands it over paused, the rest `k` as data, so two
 * programs can be compared the way normal-form bisimilarity compares
 * terms (Biernacki, Lenglet & Polesiuk, "A complete normal-form
 * bisimilarity for algebraic effects and handlers", FSCD 2020): walk
 * both to their next operation, demand the SAME operation, then feed
 * both continuations the same answer and go on. Two programs that
 * agree on every path are indistinguishable by ANY handler, because a
 * handler sees nothing but the operations and chooses nothing but the
 * answers. It is equivalence of interaction trees in the sense of Xia
 * et al. (POPL 2020), without the silent steps: `Machine.head` has
 * already run them.
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
 * The two programs' operations are compared with `==`, at the same
 * place of the row, and then each is answered from ITS OWN call to
 * `Answers`, zipped. That
 * is sound because `Answers` is a function of the operation, so equal
 * operations get equal lists. When the lists differ anyway, that is
 * reported as a `Differ` naming the answers rather than trusted.
 */
object Bisim:

  /**
   * The answers the walk feeds an operation: a finite sample, and a
   * FUNCTION of the operation. An empty list makes the operation a
   * leaf (`Stop.Now`, a raise): nothing continues after it.
   *
   * It also says WHERE the programs are stepped: a row of its effects,
   * in the order `+` wrote them — each a place, an operation answered
   * by the member at its place (the path, not a class test).
   */
  trait Answers[Es[_]]:
    /** how many effects, the places of the row */
    private[Bisim] def size: Int
    /** the keys of `Es`'s effects, in the order `+` wrote them: an operation's place its key's index */
    private[Bisim] def keys: List[AnyRef]
    /** the answers to the operation at place `at` */
    private[Bisim] def answer(at: Int, op: Any): List[Any]

    /** a row of two: each operation answered by the member at its place */
    infix def +[G[_]](g: Answers[G]): Answers[Es + G] =
      val f = this
      new Answers[Es + G]:
        private[Bisim] def size: Int = f.size + g.size
        private[Bisim] def keys: List[AnyRef] = f.keys ++ g.keys
        private[Bisim] def answer(at: Int, op: Any): List[Any] =
          if at < f.size then f.answer(at, op) else g.answer(at - f.size, op)

  object Answers:
    /**
     * The answers to one effect. THE CAST, and why it is sound: the walk
     * steps at a row it lays out itself, its places in the order of the
     * members; an operation at this member's place is one of `F`'s.
     */
    def of[F[_]](f: [X] => F[X] => List[X])(using k: okay.KeyOf[F]): Answers[F] = new Answers[F]:
      private[Bisim] def size: Int = 1
      private[Bisim] def keys: List[AnyRef] = List(k.ref)
      private[Bisim] def answer(at: Int, op: Any): List[Any] = f(op.asInstanceOf[F[Any]])

    /** every `get` answered by each sample in turn; an `update` by what its function says of each */
    def state[S](samples: S*): Answers[State % S] = of[State % S]([X] => (op: State[S, X]) => op match
      case State.Get() => samples.toList
      // an update answers what its function says from each sample
      // a set does not read the state it replaces: one answer, not one per sample
      case State.Update(State.Put(s)) => List(s)
      case State.Update(f) => samples.toList.map(s => f(s)._1))

    /** every `ask` answered by each sample in turn */
    def reader[R](samples: R*): Answers[Reader % R] = of[Reader % R]([X] => (op: Reader[R, X]) => op match
      case Reader.Ask() => samples.toList
      case Reader.Asks(f) => samples.toList.map(f))

    /** a tell has one answer, and the told value is compared as part
     * of the operation */
    def writer[W]: Answers[Writer % W] = of[Writer % W]([X] => (op: Writer[W, X]) => op match
      case Writer.Say(_) => List(()))

    /** `Stop.Now` is a leaf: both programs must stop HERE */
    val stop: Answers[Stop] = of[Stop]([X] => (op: Stop[X]) => op match
      case Stop.Now => Nil)

  /** the place of an operation the walk met: its key's index among the members' (the first of the same key) */
  private def placeOf(in: In[?, ?], keys: List[AnyRef]): Int = in match
    case at: In.At[?, ?] =>
      val i = keys.indexWhere(_ eq at.ref)
      if i < 0 then throw IllegalStateException("Bisim: an operation of an effect no member answers") else i
    case _ => throw IllegalStateException("Bisim: an operation at a place not by key")

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
  def check[Es[_], A](p: A ! Es, q: A ! Es, depth: Int = 32, budget: Int = 100_000)
                      (using ans: Answers[Es]): Verdict =
    val keys = ans.keys
    def start(r: A ! Es): () => Head[Row, A] = () => Machine.head(okay.Run.kernel[A, Es, Row](r))
    @tailrec def go(todo: List[Frame[A]], paths: Int, cut: Int): Verdict = todo match
      case Nil => Verdict.Same(paths, cut)
      case _ if paths + cut >= budget => Verdict.Same(paths, cut + todo.size)
      case Frame(path, l, r, d) :: rest => (view(l, keys), view(r, keys)) match
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
    go(List(Frame(Nil, start(p), start(q), 0)), 0, 0)

  /** one pending comparison: the path so far (newest first), the two
   * programs from there — stepped when the walk gets to them — and how
   * many operations the path has */
  private final case class Frame[A](path: List[String], l: () => Head[Row, A], r: () => Head[Row, A], d: Int)

  /** an operation at its place, and the rest — `k` as data, resumed once an answer (multi-shot) */
  private final class Met[A, X](op: X, at: Int, k: Stepping.Paused[A]):
    def show: String = String.valueOf(op)
    def sameOp(that: Met[A, ?]): Boolean = that.is(op, at)
    private def is(other: Any, place: Int): Boolean = op == other && at == place
    /** each sampled answer, rendered, with where it leads */
    def next[Es[_]](ans: Answers[Es]): List[(String, () => Head[Row, A])] =
      ans.answer(at, op).map(x => (String.valueOf(x), () => resume(x)))
    // the answers to an operation are its member's, of the operation's answer type: `Stepping.resume`'s cast sound
    private def resume(x: Any): Head[Row, A] = Stepping.resume(k, x)

  /** the head form a step leaves: a value, or an operation to answer */
  private def view[A](st: () => Head[Row, A], keys: List[AnyRef]): Either[A, Met[A, ?]] = st() match
    case Head.Done(a) => Left(a)
    case p: Head.Paused[?, ?, ?, ?, ?, ?, ?, ?, ?] => Right(Met(p.op, placeOf(p.in, keys), p))
