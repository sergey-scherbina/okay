package okay.frege

import okay.{!, %, +, Chunk, ChunkBuf, Chunks, Free, Pure, Stage, Take, TypeableK, Writer, effect, pure}
import okay.frege.Prog.{TOp, TProg}
import frege.prelude.PreludeBase.{TList, TMaybe}
import frege.prelude.PreludeBase.TST
import frege.run8.Thunk
import scala.reflect.ClassTag

/**
 * Frege programs as okay programs (specs/frege.md).
 *
 * A Frege program written in `okay.frege.Prog` — the thin Frege monad in
 * src/main/frege — is a TREE: an answer, or one operation for okay and a
 * Frege function from its answer to the rest. This driver walks it:
 * `await`/`tell` become the stage's own, `perform op` an operation of
 * the okay program's row, `liftIO io` runs existing Frege IO as one
 * step. Every step is an okay program node, so the okay interpreter
 * trampolines the walk (a hundred thousand steps on the default stack),
 * and every continuation is a Frege function — a handler that resumes
 * twice (`Choice`) simply calls it twice.
 *
 * No thread, no handoff. An earlier cut ran Frege IO on its own thread
 * as the continuation: correct, but one-shot and ~10.5 µs per element;
 * this road measured 0.25 µs per step in the same probe (Results).
 */
object Frege {

  /** a Frege value, as the type the okay side declared */
  private def as[T](x: Any, what: String)(using ct: ClassTag[T]): T = x match
    case ct(t) => t
    case other => throw IllegalArgumentException(
      s"okay.frege: $what expected ${ct.runtimeClass.getName}, got " +
        (if other == null then "null" else other.getClass.getName))

  /**
   * An answer handed to a Frege continuation, which takes `Object`: an
   * erased value already IS one at run time (a primitive arrives boxed),
   * so the ascription checks nothing and cannot fail.
   */
  private def boxed(a: Any): AnyRef = a.asInstanceOf[AnyRef]

  /** the rest of the program, given the answer to its current step */
  private def resume[A](s: TProg.DStep[A], answer: Any): TProg[A] =
    s.mem2.call().apply(Thunk.`lazy`[AnyRef](boxed(answer))).call()

  /** existing Frege IO, run as one step; its answer */
  private def lifted(l: TOp.DLift): AnyRef = TST.performUnsafe(l.mem1.call()).call()

  /**
   * A Frege `Prog ()` that uses `await` and `tell`, as a `Stage`. The
   * program is built when the stage starts, so one stage value runs
   * afresh each time; a program that returns early ends the stage, so
   * `through` pulls nothing more from upstream. It is `stageWith` at the
   * empty row: `Pure` is `Nothing`, so `Writer % O + Pure` IS
   * `Writer % O` and there is one walker, not two.
   */
  def stage[I, O: ClassTag](prog: => TProg[?], name: String = "a Frege stage"): Stage[I, O, Unit] =
    stageWith[I, O, Pure](prog, name)

  /**
   * A Frege stage that also PERFORMS: `await` and `tell` are the stage's
   * own, `perform op` an operation of the row F — a Frege filter that
   * asks its threshold from `Reader`, a stage that sleeps between
   * elements. The row is okay-stream's effectful stage row, so the
   * result runs through `through`'s G overloads like any okay stage
   * written that way.
   */
  def stageWith[I, O: ClassTag, F[+_]](prog: => TProg[?], name: String = "a Frege stage")
                                     (using Row[F]): Unit ! (Take % I + (Writer % O + F)) =
    type R = Take % I + (Writer % O + F)
    def go[A](p: TProg[A]): Unit ! R =
      val done = p.asDone()
      if done != null then pure(())
      else
        val s = p.asStep()
        val op = s.mem1.call()
        if op.asAwait() != null then
          effect[R, Option[I]](Take.Await()).flatMap(in => go(resume(s, in.fold(null)(boxed))))
        else if op.asTell() != null then
          effect[R, Unit](Writer(as[O](op.asTell().mem1.call(), s"$name's tell"))).flatMap(_ => go(resume(s, null)))
        else if op.asLift() != null then
          val l = op.asLift()
          Free.delay(() => go(resume(s, lifted(l))))
        else
          val raw = op.asPerform().mem1.call()
          operationOf[F](raw) match
            case Some(o) => effect[R, Any](o).flatMap(x => go(resume(s, x)))
            case None => throw IllegalArgumentException(
              s"okay.frege: $name performed ${raw.getClass.getName}, which is not an operation of this " +
                "stage's row (a stage's own are await and tell; stageWith[I, O, F] adds F)")
    Free.delay(() => go(prog))

  /**
   * Whether a value from Frege is an operation of the row F. The core's
   * `TypeableK` tests ONE signature (a row needs none there: `split`
   * tests one side and takes the other by exclusion); an operation
   * arriving as an `Object` has no other side to exclude, so a row is
   * tested member by member. Resolved at the concrete row of a call
   * site, never searched at an abstract one (AGENTS.md, the row crash).
   */
  trait Row[F[+_]]:
    def test(x: Any): Boolean
    /** this row and another, side by side: `Row.of[Reader % Long] |
     * Row.of[State % Long]` is the row `Reader % Long + State % Long` */
    def |[G[+_]](g: Row[G]): Row[F + G] = x => test(x) || g.test(x)

  /**
   * A single signature's row is FOUND (its `TypeableK`); a union row is
   * BUILT with `|` and passed. Not a given for `F + G`: dotty does not
   * infer F and G from a union type lambda (it answered Nothing for
   * both, measured), so a union is spelled once at the call site —
   * `Frege.run[Reader % Long + State % Long, A](p)(using Row.of[Reader % Long] | Row.of[State % Long])`.
   */
  object Row:
    given one[F[+_]](using t: TypeableK[F]): Row[F] = x => t.test(x)
    def of[F[+_]](using t: TypeableK[F]): Row[F] = one[F]

  /**
   * The one cast of this module: an operation arriving from Frege as an
   * `Object`, refined to the row after the row's own test has said it IS
   * an operation of F. `F` is covariant, so an `F[X]` is an `F[Any]` and
   * the answer needs no cast — it goes back to Frege as an `Object`. The
   * same claim `split` makes for every runner in the core.
   */
  private def operationOf[F[+_]](op: AnyRef)(using r: Row[F]): Option[F[Any]] =
    if r.test(op) then Some(op.asInstanceOf[F[Any]]) else None

  /**
   * A Frege `Prog a` as `A ! F`: each `perform op` is performed as an
   * operation of `F`, under whatever handlers run the result, and its
   * answer handed to the Frege continuation — which a multi-shot handler
   * may call more than once. An operation outside `F` is refused by
   * name; `await`/`tell` belong to `stage`.
   */
  def run[F[+_], A: ClassTag](prog: => TProg[?], name: String = "a Frege program")(using Row[F]): A ! F =
    def go[X](p: TProg[X]): A ! F =
      val done = p.asDone()
      if done != null then pure(as[A](done.mem1.call(), s"$name's answer"))
      else
        val s = p.asStep()
        val op = s.mem1.call()
        if op.asPerform() != null then
          val raw = op.asPerform().mem1.call()
          operationOf[F](raw) match
            case Some(o) => effect[F, Any](o).flatMap(x => go(resume(s, x)))
            case None => throw IllegalArgumentException(
              s"okay.frege: $name performed ${raw.getClass.getName}, which is not an operation of this program's row")
        else if op.asLift() != null then
          val l = op.asLift()
          Free.delay(() => go(resume(s, lifted(l))))
        else throw IllegalStateException(
          s"okay.frege: $name used ${if op.asAwait() != null then "await" else "tell"} outside a stage; " +
            "Frege.stage runs a program that awaits and tells")
    Free.delay(() => go(prog))

  // ------------------------------------------------------------ data

  /**
   * A Frege list as okay `Chunks`, LAZILY: each chunk forces as many
   * cells of the Frege list as it holds, when okay pulls it — an
   * infinite Frege list is fine, and nothing past the last pulled chunk
   * is ever evaluated. Each run of the result walks the list from its
   * head again (a Frege list is an immutable, memoised value, so a
   * second run is the same list, not a spent one).
   */
  def chunks[A: ClassTag](xs: => TList[?], size: Int = 64): Chunks[A] =
    def go(rest: TList[?]): Chunks[A] = Free.delay { () =>
      val buf = ChunkBuf[A](size)
      var cur = rest
      var i = 0
      while i < size && cur.asCons() != null do
        val c = cur.asCons()
        buf(i) = as[A](c.mem1.call(), "a Frege list's element")
        cur = c.mem2.call()
        i += 1
      if i == 0 then pure(())
      else
        val next = cur
        Writer.tell(buf.take(i)).flatMap(_ => go(next))
    }
    Free.delay(() => go(xs))

  /**
   * okay `Chunks` as a Frege list, LAZILY: a cell is built when Frege
   * forces it, and okay pulls the next chunk only when Frege forces past
   * the last element of the current one. PURE by type — `Chunks` is
   * `Writer % Chunk[A]` and nothing else — because an effect run from
   * inside a Frege thunk is lazy IO (specs/frege.md): an effectful okay
   * source is a `perform` in a `Prog`, not a list.
   */
  def list[A](c: Chunks[A]): TList[A] =
    def cells(chunk: Chunk[A], i: Int, rest: Chunks[A]): TList[A] =
      if i < chunk.length then
        TList.DCons.mk[A](Thunk.`lazy`[A](chunk(i)), Thunk.shared[TList[A]](() => cells(chunk, i + 1, rest)))
      else Chunks.pull(rest) match
        case Some((next, more)) => cells(next, 0, more)
        case None => TList.DList.mk[A]()
    cells(Chunks.emptyChunk[A], 0, c)

  /** Frege's `Maybe` as an `Option` */
  def option[A: ClassTag](m: TMaybe[?]): Option[A] =
    val j = m.asJust()
    if j == null then None else Some(as[A](j.mem1.call(), "a Maybe's value"))

  /** an `Option` as Frege's `Maybe` */
  def maybe[A](o: Option[A]): TMaybe[A] = o match
    case Some(a) => TMaybe.DJust.mk[A](Thunk.`lazy`[A](a))
    case None => TMaybe.DNothing.mk[A]()
}
