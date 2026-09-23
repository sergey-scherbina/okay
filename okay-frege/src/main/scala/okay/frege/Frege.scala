package okay.frege

import okay.{!, %, +, Chunk, ChunkBuf, Chunks, Foreign, Free, Member, Pure, Stage, Take, Writer, pure}
import okay.frege.Prog.TProg
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

  private def as[T: ClassTag](x: Any, what: String): T = Foreign.as[T](x, what, "okay.frege")

  /** the rest of the program, given the answer to its current step */
  private def resume[A](s: TProg.DStep[A], answer: AnyRef): TProg[A] =
    s.mem2.call().apply(Thunk.`lazy`[AnyRef](answer)).call()

  /** `Prog`'s constructors, read for okay-stream's `Foreign` walker
   * (interop-shared): `Done a`, or `Step op k` with `op` one of Await,
   * Tell, Perform and Lift */
  private given View: Foreign.View[TProg[?]] with
    def kind(p: TProg[?]): Int =
      if p.asDone() != null then Foreign.Done
      else
        val op = p.asStep().mem1.call()
        if op.asPerform() != null then Foreign.Perform
        else if op.asAwait() != null then Foreign.Await
        else if op.asTell() != null then Foreign.Tell
        else Foreign.Lift
    def payload(p: TProg[?]): AnyRef =
      val done = p.asDone()
      if done != null then Foreign.obj(done.mem1.call())
      else
        val op = p.asStep().mem1.call()
        if op.asTell() != null then op.asTell().mem1.call() else op.asPerform().mem1.call()
    /** existing Frege IO, run as one step; its answer */
    def lift(p: TProg[?]): AnyRef = TST.performUnsafe(p.asStep().mem1.call().asLift().mem1.call()).call()
    def resume(p: TProg[?], answer: AnyRef): TProg[?] = Frege.resume(p.asStep(), answer)
    /** `liftIO` on a thread of its own, which a cancel interrupts, when the
     * row carries Async (interop-lift-cancellation) */
    override def liftAsOperation(p: TProg[?]): Option[AnyRef] = Some(okay.Interruptible.await(() => lift(p)))
    def who = "okay.frege"

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
   * written that way. The walk is okay-stream's `Foreign`, shared with
   * okay-clojure's `Program`.
   */
  def stageWith[I, O: ClassTag, F[+_]](prog: => TProg[?], name: String = "a Frege stage")
                                     (using Row[F]): Unit ! (Take % I + (Writer % O + F)) =
    Foreign.stageWith[I, O, F, TProg[?]](prog, name)

  /**
   * Whether a value from Frege is an operation of the row F: the core's
   * `okay.Member` (interop-shared) under the name this module's users
   * already bind. A single signature's is FOUND; a union is BUILT with
   * `|` — `Frege.Row.of[Reader % Long] | Frege.Row.of[State % Long]` —
   * because dotty does not infer F and G from a union type lambda.
   */
  type Row[F[+_]] = Member[F]
  val Row: Member.type = Member

  /**
   * A Frege `Prog a` as `A ! F`: each `perform op` is performed as an
   * operation of `F`, under whatever handlers run the result, and its
   * answer handed to the Frege continuation — which a multi-shot handler
   * may call more than once. An operation outside `F` is refused by
   * name; `await`/`tell` belong to `stage`.
   */
  def run[F[+_], A: ClassTag](prog: => TProg[?], name: String = "a Frege program")(using Row[F]): A ! F =
    Foreign.run[F, A, TProg[?]](prog, name)

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
